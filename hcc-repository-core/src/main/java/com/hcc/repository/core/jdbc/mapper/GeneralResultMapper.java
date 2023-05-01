package com.hcc.repository.core.jdbc.mapper;

import com.hcc.repository.annotation.IEnum;
import com.hcc.repository.core.convert.ConverterFactory;
import com.hcc.repository.core.convert.ValueConverter;
import com.hcc.repository.core.jdbc.ResultMapper;
import com.hcc.repository.core.metadata.TableColumnInfo;
import com.hcc.repository.core.metadata.TableInfo;
import com.hcc.repository.core.metadata.TableInfoHelper;
import com.hcc.repository.core.utils.ReflectUtils;
import org.springframework.jdbc.support.JdbcUtils;

import java.io.Serializable;
import java.lang.reflect.Field;
import java.math.BigDecimal;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.util.Arrays;
import java.util.Map;
import java.util.Objects;

/**
 * 通用的ResultMapper转换器
 *
 * @author hushengjun
 * @date 2023/3/17
 */
public class GeneralResultMapper<T> implements ResultMapper<T> {

    private final Class<T> entityClass;

    public GeneralResultMapper(Class<T> entityClass) {
        this.entityClass = entityClass;
    }

    @Override
    @SuppressWarnings("unchecked")
    public T resultMap(ResultSet rs, int i) throws SQLException {
        T instance = ReflectUtils.newInstance(entityClass);

        TableInfo tableInfo = TableInfoHelper.getTableInfo(entityClass);
        Map<String, TableColumnInfo> columnNameColumnInfoMap = TableInfoHelper.getColumnNameColumnInfoMap(entityClass);
        ResultSetMetaData rsMetaData = rs.getMetaData();

        int columnCount = rsMetaData.getColumnCount();
        for (int index = 1; index <= columnCount; index++) {
            String columnName = this.getColumnName(rsMetaData, index);
            // 对应实体元数据
            TableColumnInfo tableColumnInfo = columnNameColumnInfoMap.get(columnName);
            if (tableColumnInfo == null) {
                continue;
            }
            Object columnValue = JdbcUtils.getResultSetValue(rs, index);
            if (columnValue == null) {
                continue;
            }

            Field field = tableColumnInfo.getField();
            Class<?> fieldTypeClass = field.getType();
            Object targetValue;

            if (tableColumnInfo.needConvert()) {
                // 用户自定义转换器
                targetValue = ReflectUtils.newInstanceForCache(tableColumnInfo.getConverter())
                        .convertToAttribute(columnValue);
            } else if (tableColumnInfo.isAssignableFromIEnum()) {
                // 枚举处理
                targetValue = this.convertToEnum(columnValue, fieldTypeClass);
            } else {
                ValueConverter<?> converter = ConverterFactory.getConverter(fieldTypeClass);
                if (converter != null) {
                    // 可以获取到转换器，则使用转换器转换
                    targetValue = converter.convert(columnValue, fieldTypeClass);
                } else {
                    // 否则使用的是ResultSet提供的方法获取值
                    targetValue = JdbcUtils.getResultSetValue(rs, index, fieldTypeClass);
                }
            }

            // 执行监听器
            targetValue = ReflectUtils.newInstanceForCache(tableInfo.getPropSet())
                    .onPropSet(instance, targetValue, field.getName(), columnName);

            // 反射赋值
            ReflectUtils.setValue(instance, field, targetValue);
        }

        return instance;
    }

    /**
     * 转换到枚举
     * @param val
     * @param enumClass
     * @param <E>
     * @return
     */
    private <E> E convertToEnum(Object val, Class<E> enumClass) {
        E[] enumConstants = enumClass.getEnumConstants();
        return Arrays.stream(enumConstants)
                .filter(e -> {
                    Serializable value = ((IEnum<?>) e).getValue();
                    if (value == null) {
                        return false;
                    }
                    if (value instanceof BigDecimal) {
                        return new BigDecimal(val.toString()).compareTo((BigDecimal) value) == 0;
                    }

                    return Objects.equals(val.toString(), value.toString());
                })
                .findFirst()
                .orElse(null);
    }

}
