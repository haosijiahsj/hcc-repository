package com.hcc.repository.core.convert.converter;

import com.hcc.repository.core.convert.ValueConverter;

import java.sql.Date;
import java.sql.Timestamp;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;

/**
 * LocalDateTimeConverter
 *
 * @author hushengjun
 * @date 2023/3/24
 */
public class LocalDateConverter implements ValueConverter<LocalDate> {

    private static final DateTimeFormatter DATE_FORMATTER = DateTimeFormatter.ofPattern("yyyy-MM-dd");

    @Override
    public LocalDate convert(Object value, Class<?> targetClass) {
        if (value instanceof String) {
            return LocalDate.parse((String) value, DATE_FORMATTER);
        } else if (value instanceof Timestamp) {
            return ((Timestamp)value).toLocalDateTime().toLocalDate();
        } else if (value instanceof Date) {
            return ((Date)value).toLocalDate();
        }

        throw new RuntimeException(String.format("无法转换值：%s，到目标class: %s", value, targetClass.getName()));
    }

}
