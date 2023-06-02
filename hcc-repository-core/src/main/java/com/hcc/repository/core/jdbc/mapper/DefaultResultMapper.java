package com.hcc.repository.core.jdbc.mapper;

import com.hcc.repository.core.jdbc.ResultMapper;

import java.math.BigDecimal;
import java.sql.Blob;
import java.sql.Clob;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.util.Arrays;
import java.util.Date;
import java.util.List;
import java.util.Map;

/**
 * 默认的结果映射器，通过targetClass类型选择合适的映射器
 *
 * @author hushengjun
 * @date 2023/6/2
 */
public class DefaultResultMapper implements ResultMapper<Object> {

    private static final List<Class<?>> SUPPORT_OBJ_CLASSES;
    private static final List<Class<?>> SUPPORT_JAVA8_TIME_CLASSES;

    private final Class<?> targetClass;
    private final ResultMapper<?> resultMapper;

    static {
        SUPPORT_OBJ_CLASSES = Arrays.asList(
                boolean.class, byte.class, short.class, int.class, long.class, float.class, double.class, char.class,
                Boolean.class, Byte.class, Short.class, Integer.class, Long.class, Float.class, Double.class, Character.class,
                Number.class, BigDecimal.class,
                Date.class, java.sql.Date.class, java.sql.Time.class, java.sql.Timestamp.class,
                byte[].class, Blob.class, Clob.class,
                String.class
        );
        SUPPORT_JAVA8_TIME_CLASSES = Arrays.asList(
                LocalTime.class, LocalDate.class, LocalDateTime.class
        );
    }

    public DefaultResultMapper(Class<?> targetClass) {
        this.targetClass = targetClass;
        this.resultMapper = this.matchResultMapper();
    }

    /**
     * 匹配映射器
     * @return
     */
    private ResultMapper<?> matchResultMapper() {
        if (Map.class.isAssignableFrom(targetClass)) {
            return new MapResultMapper();
        } else if (SUPPORT_OBJ_CLASSES.contains(targetClass)) {
            return new ObjectResultMapper(targetClass);
        } else if (SUPPORT_JAVA8_TIME_CLASSES.contains(targetClass)) {
            return new Java8TimeResultMapper(targetClass);
        }

        return new EntityResultMapper<>(targetClass);
    }
    @Override
    public Object resultMap(ResultSet rs, int rowNum) throws SQLException {
        return resultMapper.resultMap(rs, rowNum);
    }

}
