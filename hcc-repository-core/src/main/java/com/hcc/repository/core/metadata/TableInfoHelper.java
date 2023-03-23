package com.hcc.repository.core.metadata;

import com.hcc.repository.annotation.Column;
import com.hcc.repository.annotation.Id;
import com.hcc.repository.annotation.Table;
import com.hcc.repository.core.utils.CollUtils;
import com.hcc.repository.core.utils.StrUtils;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 * TableInfoCache
 *
 * @author hushengjun
 * @date 2023/3/17
 */
public class TableInfoHelper {

    private static final Map<Class<?>, TableInfo> CACHE = new ConcurrentHashMap<>(128);

    private TableInfoHelper() {}

    public static TableInfo getTableInfo(Class<?> clazz) {
        if (clazz == null) {
            throw new NullPointerException();
        }

        TableInfo tableInfo = CACHE.get(clazz);
        if (tableInfo != null) {
            return tableInfo;
        }

        tableInfo = new TableInfo();
        Table tableAnnotation = clazz.getAnnotation(Table.class);
        tableInfo.setClazz(clazz);
        tableInfo.setTableName(resolveTableName(clazz));
        if (tableAnnotation != null) {
            tableInfo.setColumnPrefix(tableAnnotation.columnPrefix());
            tableInfo.setIgnorePropertyNames(Arrays.asList(tableAnnotation.ignorePropertyNames()));
        }

        List<TableColumnInfo> tableColumnInfos = new ArrayList<>();
        Field[] fields = clazz.getDeclaredFields();
        for (Field field : fields) {
            String fieldName = field.getName();
            if (CollUtils.isNotEmpty(tableInfo.getIgnorePropertyNames()) && tableInfo.getIgnorePropertyNames().contains(fieldName)) {
                continue;
            }

            TableColumnInfo tableColumnInfo = new TableColumnInfo();
            tableColumnInfo.setField(field);
            tableColumnInfo.setFieldName(fieldName);

            Id idAnnotation = field.getAnnotation(Id.class);
            if (idAnnotation != null) {
                if (tableInfo.getIdColumnInfo() != null) {
                    throw new IllegalArgumentException("定义了重复id列");
                }
                String idName = StrUtils.isEmpty(idAnnotation.value()) ? StrUtils.humpToUnderline(fieldName) : idAnnotation.value();
                tableColumnInfo.setColumnName(tableInfo.getColumnPrefix() + idName);
                tableColumnInfo.setIdType(idAnnotation.idType());
                tableColumnInfo.setGenerator(idAnnotation.generator());
                tableColumnInfo.setPrimaryKey(true);

                tableInfo.setIdColumnInfo(tableColumnInfo);

                tableColumnInfos.add(tableColumnInfo);
                continue;
            }
            Column columnAnnotation = field.getAnnotation(Column.class);
            if (columnAnnotation != null) {
                if (columnAnnotation.ignore()) {
                    continue;
                }
                String columnName = StrUtils.isEmpty(columnAnnotation.value()) ? StrUtils.humpToUnderline(fieldName) : columnAnnotation.value();
                tableColumnInfo.setColumnName(columnName);
                tableColumnInfo.setConverter(columnAnnotation.converter());
            } else {
                tableColumnInfo.setColumnName(StrUtils.humpToUnderline(fieldName));
            }

            tableColumnInfos.add(tableColumnInfo);
        }
        tableInfo.setColumnInfos(tableColumnInfos);

        CACHE.put(clazz, tableInfo);

        return tableInfo;
    }

    /**
     * 获取表名
     * @param clazz
     * @return
     */
    private static String resolveTableName(Class<?> clazz) {
        Table tableAnno = clazz.getAnnotation(Table.class);
        // 没有该注解的将类名转为下划线作为表名
        if (tableAnno == null) {
            return StrUtils.humpToUnderline(clazz.getSimpleName());
        }

        return StrUtils.isEmpty(tableAnno.value()) ? clazz.getSimpleName() : tableAnno.value();
    }

    public static TableColumnInfo getIdColumnInfo(Class<?> clazz) {
        return getTableInfo(clazz).getIdColumnInfo();
    }

    public static String getTableName(Class<?> clazz) {
        return getTableInfo(clazz).getTableName();
    }

    public static String getIdColumnName(Class<?> clazz) {
        TableColumnInfo idColumnInfo = getIdColumnInfo(clazz);
        if (idColumnInfo == null) {
            throw new RuntimeException("没有id");
        }
        return idColumnInfo.getColumnName();
    }

    public static List<TableColumnInfo> getColumnInfos(Class<?> clazz) {
        return getTableInfo(clazz).getColumnInfos();
    }

    public static List<TableColumnInfo> getColumnInfosWithOutIdColumn(Class<?> clazz) {
        return getTableInfo(clazz).getColumnInfos().stream().filter(c -> !c.isPrimaryKey()).collect(Collectors.toList());
    }

    public static Map<String, TableColumnInfo> getFieldNameColumnInfoMap(Class<?> clazz) {
        List<TableColumnInfo> columnInfos = getTableInfo(clazz).getColumnInfos();
        return Optional.ofNullable(columnInfos)
                .orElse(new ArrayList<>())
                .stream()
                .collect(Collectors.toMap(TableColumnInfo::getFieldName, Function.identity()));
    }

    public static Map<String, TableColumnInfo> getColumnNameColumnInfoMap(Class<?> clazz) {
        List<TableColumnInfo> columnInfos = getTableInfo(clazz).getColumnInfos();
        return Optional.ofNullable(columnInfos)
                .orElse(new ArrayList<>())
                .stream()
                .collect(Collectors.toMap(TableColumnInfo::getColumnName, Function.identity()));
    }

    public static TableColumnInfo getColumnInfoByClassAndFieldName(Class<?> clazz, String fieldName) {
        Map<String, TableColumnInfo> fieldNameColumnInfoMap = getFieldNameColumnInfoMap(clazz);
        if (CollUtils.isEmpty(fieldNameColumnInfoMap)) {
            return null;
        }

        return fieldNameColumnInfoMap.get(fieldName);
    }

    public static boolean hasIdColumn(Class<?> clazz) {
        return getIdColumnInfo(clazz) != null;
    }

}
