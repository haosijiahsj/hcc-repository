package com.hcc.repository.extension.converter.json;

import com.google.gson.Gson;

/**
 * GsonConverter
 *
 * @author hushengjun
 * @date 2023/7/19
 */
public class GsonConverter implements JsonConverter<Object> {

    private static final Gson GSON = new Gson();

    private final Class<?> targetClass;

    public GsonConverter(Class<?> targetClass) {
        this.targetClass = targetClass;
    }

    @Override
    public String toJson(Object attribute) {
        return GSON.toJson(attribute);
    }

    @Override
    public Object parseJson(String json) {
        return GSON.fromJson(json, targetClass);
    }

}
