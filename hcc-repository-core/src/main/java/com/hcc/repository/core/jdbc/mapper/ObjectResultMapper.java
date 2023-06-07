package com.hcc.repository.core.jdbc.mapper;

import com.hcc.repository.core.exceptions.RepositoryException;
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
public class ObjectResultMapper implements ResultMapper<Object> {

    private final Class<?> targetClass;

    public ObjectResultMapper(Class<?> targetClass) {
        this.targetClass = targetClass;
    }

    @Override
    public Object resultMap(ResultSet rs, int rowNum) throws SQLException {
        ResultSetMetaData metaData = rs.getMetaData();
        if (metaData.getColumnCount() != 1) {
            throw new RepositoryException("列不唯一");
        }
        Object value = rs.getObject(1);
        if (value == null) {
            return null;
        }

        return JdbcUtils.getResultSetValue(rs, 1, targetClass);
    }

}
