package com.hcc.repository.core.convert;

import com.hcc.repository.annotation.IConverter;
import com.hcc.repository.core.exceptions.RepositoryException;

import java.sql.Timestamp;
import java.time.LocalTime;
import java.time.format.DateTimeFormatter;

/**
 * LocalTimeConverter
 *
 * @author hushengjun
 * @date 2023/3/24
 */
public class LocalTimeConverter implements IConverter<LocalTime, Object> {

    private static final DateTimeFormatter TIME_FORMATTER = DateTimeFormatter.ofPattern("HH:mm:ss");

    @Override
    public LocalTime convertToAttribute(Object value) {
        if (value instanceof String) {
            return LocalTime.parse((String) value, TIME_FORMATTER);
        } else if (value instanceof Timestamp) {
            return ((Timestamp)value).toLocalDateTime().toLocalTime();
        }

        throw new RepositoryException(String.format("无法转换值：%s，到目标LocalTime", value));
    }

}
