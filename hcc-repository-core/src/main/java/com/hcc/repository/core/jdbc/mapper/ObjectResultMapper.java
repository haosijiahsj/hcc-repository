package com.hcc.repository.core.jdbc.mapper;

import com.hcc.repository.core.jdbc.ResultMapper;
import org.springframework.jdbc.support.JdbcUtils;

import java.sql.ResultSet;
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
        return JdbcUtils.getResultSetValue(rs, 1, targetClass);
    }

}
