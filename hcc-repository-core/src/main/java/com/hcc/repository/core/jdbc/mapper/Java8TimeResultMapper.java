package com.hcc.repository.core.jdbc.mapper;

import com.hcc.repository.core.convert.ConverterFactory;
import com.hcc.repository.core.convert.ValueConverter;
import com.hcc.repository.core.jdbc.ResultMapper;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.time.temporal.Temporal;

/**
 * Java8日期映射器
 *
 * @author hushengjun
 * @date 2023/6/2
 */
public class Java8TimeResultMapper implements ResultMapper<Temporal> {

    private final Class<?> targetClass;
    private final ValueConverter<?> converter;

    public Java8TimeResultMapper(Class<?> targetClass) {
        this.targetClass = targetClass;
        this.converter = ConverterFactory.getConverter(targetClass);
    }

    @Override
    public Temporal resultMap(ResultSet rs, int rowNum) throws SQLException {
        return (Temporal) converter.convert(rs.getObject(1), targetClass);
    }

}
