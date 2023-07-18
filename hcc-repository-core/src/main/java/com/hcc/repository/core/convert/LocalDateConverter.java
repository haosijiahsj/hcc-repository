package com.hcc.repository.core.convert;

import com.hcc.repository.annotation.IConverter;
import com.hcc.repository.core.exceptions.RepositoryException;

import java.sql.Date;
import java.sql.Timestamp;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;

/**
 * LocalDateConverter
 *
 * @author hushengjun
 * @date 2023/3/24
 */
public class LocalDateConverter implements IConverter<LocalDate, Object> {

    private static final DateTimeFormatter DATE_FORMATTER = DateTimeFormatter.ofPattern("yyyy-MM-dd");

    @Override
    public LocalDate convertToAttribute(Object value) {
        if (value instanceof String) {
            return LocalDate.parse((String) value, DATE_FORMATTER);
        } else if (value instanceof Timestamp) {
            return ((Timestamp)value).toLocalDateTime().toLocalDate();
        } else if (value instanceof Date) {
            return ((Date)value).toLocalDate();
        }

        throw new RepositoryException(String.format("无法转换值：%s，到LocalDate", value));
    }

}
