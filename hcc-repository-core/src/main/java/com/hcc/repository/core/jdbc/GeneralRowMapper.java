package com.hcc.repository.core.jdbc;

import com.hcc.repository.annotation.IConverter;
import com.hcc.repository.core.metadata.TableColumnInfo;
import com.hcc.repository.core.metadata.TableInfoHelper;
import com.hcc.repository.core.utils.ReflectUtils;
import org.springframework.jdbc.core.RowMapper;
import org.springframework.jdbc.support.JdbcUtils;

import java.lang.reflect.Field;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.util.Map;

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
    public T mapRow(ResultSet rs, int i) throws SQLException {
        T instance;
        try {
            instance = entityClass.newInstance();
        } catch (InstantiationException | IllegalAccessException e) {
            throw new IllegalArgumentException(String.format("class: [%s]无法被实例化！", entityClass), e);
        }

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

            Field field = tableColumnInfo.getField();
            Object targetValue;
            if (tableColumnInfo.needConvert()) {
                Object columnValue = JdbcUtils.getResultSetValue(rs, index);
                targetValue = this.convertValueByCustomerConverter(columnValue, tableColumnInfo.getConverter());
            } else {
                // 是默认的，需要走内部的转换器
                targetValue = JdbcUtils.getResultSetValue(rs, index, field.getType());
                // TODO 还是得自己转哦
            }

            ReflectUtils.setValue(instance, field, targetValue);
        }

        return instance;
    }

    private String getColumnName(ResultSetMetaData resultSetMetaData, int columnIndex) throws SQLException {
        String name = resultSetMetaData.getColumnLabel(columnIndex);
        if (name == null || name.length() < 1) {
            name = resultSetMetaData.getColumnName(columnIndex);
        }

        return name;
    }

    /**
     * 转换到用户自定义类型
     * @param originalValue
     * @param converterClass
     * @return
     */
    private Object convertValueByCustomerConverter(Object originalValue, Class<? extends IConverter> converterClass) {
        return ReflectUtils.newInstance(converterClass).convertToAttribute(originalValue);
    }

}
