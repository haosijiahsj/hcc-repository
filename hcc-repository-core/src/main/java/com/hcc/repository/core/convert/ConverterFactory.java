package com.hcc.repository.core.convert;

import com.hcc.repository.core.convert.converter.LocalDateConverter;
import com.hcc.repository.core.convert.converter.LocalDateTimeConverter;
import com.hcc.repository.core.convert.converter.LocalTimeConverter;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.util.HashMap;
import java.util.Map;

/**
 * ConverterFactory
 *
 * @author hushengjun
 * @date 2023/3/24
 */
public class ConverterFactory {

    private static final Map<Class<?>, ValueConverter<?>> CONVERTER_MAP = new HashMap<Class<?>, ValueConverter<?>>() {{
        put(LocalDateTime.class, new LocalDateTimeConverter());
        put(LocalDate.class, new LocalDateConverter());
        put(LocalTime.class, new LocalTimeConverter());
    }};

    public static ValueConverter<?> getConverter(Class<?> clazz) {
        return CONVERTER_MAP.get(clazz);
    }

}
