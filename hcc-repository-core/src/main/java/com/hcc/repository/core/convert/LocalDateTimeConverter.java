package com.hcc.repository.core.convert;

import com.hcc.repository.annotation.IConverter;
import com.hcc.repository.core.exceptions.RepositoryException;

import java.sql.Timestamp;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;

/**
 * LocalDateTimeConverter
 *
 * @author hushengjun
 * @date 2023/3/24
 */
public class LocalDateTimeConverter implements IConverter<LocalDateTime, Object> {

    private static final DateTimeFormatter DATE_TIME_FORMATTER = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss");

    @Override
    public LocalDateTime convertToAttribute(Object value) {
        if (value instanceof String) {
            return LocalDateTime.parse((String) value, DATE_TIME_FORMATTER);
        } else if (value instanceof Timestamp) {
            return ((Timestamp)value).toLocalDateTime();
        }

        throw new RepositoryException(String.format("无法转换值：%s，到目标LocalDateTime", value));
    }

    @Override
    public Object convertToColumn(LocalDateTime attribute) {
        return attribute.format(DATE_TIME_FORMATTER);
    }
}
