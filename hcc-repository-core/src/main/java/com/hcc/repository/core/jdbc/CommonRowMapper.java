package com.hcc.repository.core.jdbc;

import org.springframework.jdbc.core.RowMapper;

import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;

/**
 * CommonRowMapper
 *
 * @author hushengjun
 * @date 2023/3/17
 */
public class CommonRowMapper<T> implements RowMapper<T> {

    private Class<T> entityClass;

    public CommonRowMapper(Class<T> entityClass) {
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

        ResultSetMetaData rsmd = rs.getMetaData();
        int columnCount = rsmd.getColumnCount();
        for (int index = 1; index <= columnCount; index++) {
            Object columnValue = rs.getObject(index);
            String columnName = this.getColumnName(rsmd, index);
            // 可找到对于实体的字段了
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

}
