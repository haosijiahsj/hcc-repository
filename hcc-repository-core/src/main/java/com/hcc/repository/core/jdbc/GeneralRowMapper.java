package com.hcc.repository.core.jdbc;

import com.hcc.repository.annotation.IEnum;
import com.hcc.repository.core.convert.ConverterFactory;
import com.hcc.repository.core.convert.ValueConverter;
import com.hcc.repository.core.metadata.TableColumnInfo;
import com.hcc.repository.core.metadata.TableInfoHelper;
import com.hcc.repository.core.utils.ReflectUtils;
import org.springframework.jdbc.core.RowMapper;
import org.springframework.jdbc.support.JdbcUtils;

import java.io.Serializable;
import java.lang.reflect.Field;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.util.Arrays;
import java.util.Map;
import java.util.Objects;

/**
 * 通用的rowMapper转换器
 *
 * @author hushengjun
 * @date 2023/3/17
 */
public class GeneralRowMapper<T> implements RowMapper<T> {

    private final Class<T> entityClass;

    public GeneralRowMapper(Class<T> entityClass) {
        this.entityClass = entityClass;
    }

    @Override
    @SuppressWarnings("unchecked")
    public T mapRow(ResultSet rs, int i) throws SQLException {
        T instance = ReflectUtils.newInstance(entityClass);

        Map<String, TableColumnInfo> columnNameColumnInfoMap = TableInfoHelper.getColumnNameColumnInfoMap(entityClass);
        ResultSetMetaData rsmd = rs.getMetaData();

        int columnCount = rsmd.getColumnCount();
        for (int index = 1; index <= columnCount; index++) {
            String columnName = this.getColumnName(rsmd, index);
            // 可找到对应实体的字段了
            TableColumnInfo tableColumnInfo = columnNameColumnInfoMap.get(columnName);
            if (tableColumnInfo == null) {
                continue;
            }
            Object columnValue = JdbcUtils.getResultSetValue(rs, index);
            if (columnValue == null) {
                continue;
            }

            Field field = tableColumnInfo.getField();
            Object targetValue;
            if (tableColumnInfo.needConvert()) {
                // 用户自定义转换器
                targetValue = ReflectUtils.newInstanceForCache(tableColumnInfo.getConverter())
                        .convertToAttribute(columnValue);
            } else if (tableColumnInfo.isAssignableFromIEnum()) {
                // 枚举处理
                targetValue = this.convertToEnum(columnValue, field.getType());
            } else {
                ValueConverter<?> converter = ConverterFactory.getConverter(field.getType());
                if (converter != null) {
                    // 可以获取到转换器，则使用转换器转换
                    targetValue = converter.convert(columnValue, field.getType());
                } else {
                    // 否则使用的是ResultSet提供的方法获取值
                    targetValue = JdbcUtils.getResultSetValue(rs, index, field.getType());
                }
            }

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

                    return Objects.equals(val.toString(), value.toString());
                })
                .findFirst()
                .orElse(null);
    }

    /**
     * 获取列名
     * @param resultSetMetaData
     * @param columnIndex
     * @return
     * @throws SQLException
     */
    private String getColumnName(ResultSetMetaData resultSetMetaData, int columnIndex) throws SQLException {
        String name = resultSetMetaData.getColumnLabel(columnIndex);
        if (name == null || name.length() < 1) {
            name = resultSetMetaData.getColumnName(columnIndex);
        }

        return name;
    }

}
