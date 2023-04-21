package com.hcc.repository.core.metadata;

import com.hcc.repository.annotation.Column;
import com.hcc.repository.annotation.Id;
import com.hcc.repository.annotation.LogicDelete;
import com.hcc.repository.annotation.Table;
import com.hcc.repository.core.utils.CollUtils;
import com.hcc.repository.core.utils.ReflectUtils;
import com.hcc.repository.core.utils.StrUtils;

import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
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

    /**
     * 实体元数据缓存
     */
    private static final Map<Class<?>, TableInfo> CACHE = new ConcurrentHashMap<>(128);

    private TableInfoHelper() {}

    /**
     * 批量载入缓存
     * @param classes
     */
    public static synchronized void loadAll(Collection<Class<?>> classes) {
        classes.forEach(TableInfoHelper::load);
    }

    /**
     * 载入缓存
     * @param clazz
     */
    public static synchronized TableInfo load(Class<?> clazz) {
        if (clazz == null) {
            return null;
        }

        TableInfo tableInfo = new TableInfo();
        Table tableAnnotation = clazz.getAnnotation(Table.class);
        tableInfo.setClazz(clazz);
        tableInfo.setTableName(resolveTableName(clazz));
        if (tableAnnotation != null) {
            tableInfo.setColumnPrefix(tableAnnotation.columnPrefix());
            tableInfo.setIgnorePropertyNames(Arrays.asList(tableAnnotation.ignorePropertyNames()));
        }

        List<TableColumnInfo> tableColumnInfos = new ArrayList<>();
        List<Field> fields = ReflectUtils.getAllDeclaredFields(clazz);
        for (Field field : fields) {
            // 被final和static修饰的字段不处理映射关系
            if (Modifier.isFinal(field.getModifiers()) || Modifier.isStatic(field.getModifiers())) {
                continue;
            }
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
                tableColumnInfo.setUseSingletonIdGenerator(idAnnotation.useSingletonIdGenerator());

                tableInfo.setIdColumnInfo(tableColumnInfo);
                tableInfo.setHasIdColumn(true);

                tableColumnInfos.add(tableColumnInfo);
                continue;
            }
            Column columnAnnotation = field.getAnnotation(Column.class);
            LogicDelete logicDeleteAnnotation = field.getAnnotation(LogicDelete.class);
            if (columnAnnotation != null) {
                if (columnAnnotation.ignore()) {
                    continue;
                }
                String columnName = StrUtils.isEmpty(columnAnnotation.value()) ? StrUtils.humpToUnderline(fieldName) : columnAnnotation.value();
                tableColumnInfo.setColumnName(columnName);
                tableColumnInfo.setConverter(columnAnnotation.converter());
                tableColumnInfo.setInsertStrategy(columnAnnotation.insertStrategy());
                tableColumnInfo.setUpdateStrategy(columnAnnotation.updateStrategy());
            } else {
                tableColumnInfo.setColumnName(StrUtils.humpToUnderline(fieldName));
            }
            if (logicDeleteAnnotation != null) {
                tableColumnInfo.setLogicDelete(true);
                tableColumnInfo.setLogicNotDelVal(logicDeleteAnnotation.value());
                tableColumnInfo.setLogicDelVal(logicDeleteAnnotation.delValue());
                tableColumnInfo.setLogicDelValueType(logicDeleteAnnotation.logicDelValueType());
                tableInfo.setHasLogicDeleteColumn(true);
            }

            tableColumnInfos.add(tableColumnInfo);
        }
        tableInfo.setColumnInfos(tableColumnInfos);

        CACHE.put(clazz, tableInfo);

        return tableInfo;
    }

    /**
     * 获取表元数据
     * @param clazz
     * @return
     */
    public static TableInfo getTableInfo(Class<?> clazz) {
        if (clazz == null) {
            throw new NullPointerException();
        }

        return CACHE.getOrDefault(clazz, load(clazz));
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

    /**
     * 获取id列
     * @param clazz
     * @return
     */
    public static TableColumnInfo getIdColumnInfo(Class<?> clazz) {
        return getTableInfo(clazz).getIdColumnInfo();
    }

    /**
     * 获取逻辑删除列
     * @param clazz
     * @return
     */
    public static TableColumnInfo getLogicDeleteColumnInfo(Class<?> clazz) {
        for (TableColumnInfo columnInfo : getTableInfo(clazz).getColumnInfos()) {
            if (columnInfo.isLogicDelete()) {
                return columnInfo;
            }
        }

        return null;
    }

    /**
     * 获取表名
     * @param clazz
     * @return
     */
    public static String getTableName(Class<?> clazz) {
        return getTableInfo(clazz).getTableName();
    }

    /**
     * 获取id列名
     * @param clazz
     * @return
     */
    public static String getIdColumnName(Class<?> clazz) {
        TableColumnInfo idColumnInfo = getIdColumnInfo(clazz);
        if (idColumnInfo == null) {
            throw new RuntimeException("没有id");
        }
        return idColumnInfo.getColumnName();
    }

    /**
     * 获取列元数据
     * @param clazz
     * @return
     */
    public static List<TableColumnInfo> getColumnInfos(Class<?> clazz) {
        return getTableInfo(clazz).getColumnInfos();
    }

    /**
     * 获取列元数据，排除主键列
     * @param clazz
     * @return
     */
    public static List<TableColumnInfo> getColumnInfosWithOutIdColumn(Class<?> clazz) {
        return getTableInfo(clazz).getColumnInfos().stream().filter(c -> !c.isPrimaryKey()).collect(Collectors.toList());
    }

    /**
     * 获取字段和列映射关系
     * @param clazz
     * @return
     */
    public static Map<String, TableColumnInfo> getFieldNameColumnInfoMap(Class<?> clazz) {
        List<TableColumnInfo> columnInfos = getTableInfo(clazz).getColumnInfos();
        return Optional.ofNullable(columnInfos)
                .orElse(new ArrayList<>())
                .stream()
                .collect(Collectors.toMap(TableColumnInfo::getFieldName, Function.identity()));
    }

    /**
     * 获取列名和列映射关系
     * @param clazz
     * @return
     */
    public static Map<String, TableColumnInfo> getColumnNameColumnInfoMap(Class<?> clazz) {
        List<TableColumnInfo> columnInfos = getTableInfo(clazz).getColumnInfos();
        return Optional.ofNullable(columnInfos)
                .orElse(new ArrayList<>())
                .stream()
                .collect(Collectors.toMap(TableColumnInfo::getColumnName, Function.identity()));
    }

    /**
     * 通过字段获取列元数据信息
     * @param clazz
     * @param fieldName
     * @return
     */
    public static TableColumnInfo getColumnInfoByClassAndFieldName(Class<?> clazz, String fieldName) {
        Map<String, TableColumnInfo> fieldNameColumnInfoMap = getFieldNameColumnInfoMap(clazz);
        if (CollUtils.isEmpty(fieldNameColumnInfoMap)) {
            return null;
        }

        return fieldNameColumnInfoMap.get(fieldName);
    }

    /**
     * 判断是否有id列
     * @param clazz
     * @return
     */
    public static boolean hasIdColumn(Class<?> clazz) {
        return getIdColumnInfo(clazz) != null;
    }

    /**
     * 判断是否有逻辑删除列信息
     * @param clazz
     * @return
     */
    public static boolean hasLogicDeleteColumn(Class<?> clazz) {
        return getTableInfo(clazz).isHasLogicDeleteColumn();
    }

}
