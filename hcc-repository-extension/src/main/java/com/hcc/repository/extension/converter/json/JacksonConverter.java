package com.hcc.repository.extension.converter.json;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;

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
    public String toJson(Object attribute) {
        try {
            return OBJECT_MAPPER.writeValueAsString(attribute);
        } catch (JsonProcessingException e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public Object parseJson(String json) {
        try {
            return OBJECT_MAPPER.readValue(json, targetClass);
        } catch (JsonProcessingException e) {
            throw new RuntimeException(e);
        }
    }

}
