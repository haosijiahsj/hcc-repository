package com.hcc.repository.core.jdbc;

import org.springframework.jdbc.core.RowMapper;

import java.sql.ResultSet;
import java.sql.SQLException;

/**
 * RowMapper包装类
 *
 * @author hushengjun
 * @date 2023/6/9
 */
public class RowMapperWrapper<T> implements RowMapper<T> {

    private final ResultMapper<T> resultMapper;

    public RowMapperWrapper(ResultMapper<T> resultMapper) {
        this.resultMapper = resultMapper;
    }

    public static <T> RowMapperWrapper<T> create(ResultMapper<T> resultMapper) {
        return new RowMapperWrapper<>(resultMapper);
    }

    @Override
    public T mapRow(ResultSet rs, int rowNum) throws SQLException {
        return resultMapper.resultMap(rs, rowNum);
    }

}
