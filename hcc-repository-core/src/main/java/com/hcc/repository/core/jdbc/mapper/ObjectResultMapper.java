package com.hcc.repository.core.jdbc.mapper;

import com.hcc.repository.core.exceptions.IncorrectColumnCountException;
import com.hcc.repository.core.jdbc.ResultMapper;
import com.hcc.repository.core.utils.JdbcUtils;

import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;

/**
 * 对象映射器
 *
 * @author hushengjun
 * @date 2023/6/2
 */
public class ObjectResultMapper<T> implements ResultMapper<T> {

    private final Class<T> targetClass;

    public ObjectResultMapper(Class<T> targetClass) {
        this.targetClass = targetClass;
    }

    @Override
    public T resultMap(ResultSet rs, int rowNum) throws SQLException {
        ResultSetMetaData metaData = rs.getMetaData();
        if (metaData.getColumnCount() != 1) {
            throw new IncorrectColumnCountException("列不唯一");
        }
        Object value = rs.getObject(1);
        if (value == null) {
            return null;
        }

        return (T) JdbcUtils.getResultSetValue(rs, 1, targetClass);
    }

}
