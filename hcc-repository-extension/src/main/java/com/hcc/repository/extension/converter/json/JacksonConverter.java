package com.hcc.repository.extension.converter.json;

import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.SneakyThrows;

/**
 * JacksonConverter
 *
 * @author hushengjun
 * @date 2023/7/19
 */
public class JacksonConverter implements JsonConverter<Object> {

    private static final ObjectMapper OBJECT_MAPPER = new ObjectMapper();

    private final Class<?> targetClass;

    public JacksonConverter(Class<?> targetClass) {
        this.targetClass = targetClass;
    }

    @Override
    @SneakyThrows
    public String toJson(Object attribute) {
        return OBJECT_MAPPER.writeValueAsString(attribute);
    }

    @Override
    @SneakyThrows
    public Object parseJson(String json) {
        return OBJECT_MAPPER.readValue(json, targetClass);
    }

}
