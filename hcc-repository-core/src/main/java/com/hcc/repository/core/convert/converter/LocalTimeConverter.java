package com.hcc.repository.core.convert.converter;

import com.hcc.repository.core.convert.ValueConverter;
import com.hcc.repository.core.exceptions.RepositoryException;

import java.sql.Timestamp;
import java.time.LocalTime;
import java.time.format.DateTimeFormatter;

/**
 * LocalDateTimeConverter
 *
 * @author hushengjun
 * @date 2023/3/24
 */
public class LocalTimeConverter implements ValueConverter<LocalTime> {

    private static final DateTimeFormatter TIME_FORMATTER = DateTimeFormatter.ofPattern("HH:mm:ss");

    @Override
    public LocalTime convert(Object value, Class<?> targetClass) {
        if (value instanceof String) {
            return LocalTime.parse((String) value, TIME_FORMATTER);
        } else if (value instanceof Timestamp) {
            return ((Timestamp)value).toLocalDateTime().toLocalTime();
        }

        throw new RepositoryException(String.format("无法转换值：%s，到目标class: %s", value, targetClass.getName()));
    }

}
