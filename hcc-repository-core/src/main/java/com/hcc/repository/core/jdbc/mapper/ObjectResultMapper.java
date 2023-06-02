package com.hcc.repository.core.jdbc.mapper;

import com.hcc.repository.core.convert.ConverterFactory;
import com.hcc.repository.core.convert.ValueConverter;
import com.hcc.repository.core.exceptions.RepositoryException;
import com.hcc.repository.core.jdbc.ResultMapper;
import org.springframework.jdbc.support.JdbcUtils;

import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.util.Arrays;
import java.util.List;

/**
 * 对象映射器
 *
 * @author hushengjun
 * @date 2023/6/2
 */
public class ObjectResultMapper implements ResultMapper<Object> {

    private static final List<Class<?>> SUPPORT_JAVA8_TIME_CLASSES;

    static {
        SUPPORT_JAVA8_TIME_CLASSES = Arrays.asList(
                LocalTime.class, LocalDate.class, LocalDateTime.class
        );
    }

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

        // java8日期单独处理
        if (SUPPORT_JAVA8_TIME_CLASSES.contains(targetClass)) {
            ValueConverter<?> converter = ConverterFactory.getConverter(targetClass);
            return converter.convert(value, targetClass);
        }

        return JdbcUtils.getResultSetValue(rs, 1, targetClass);
    }

}
