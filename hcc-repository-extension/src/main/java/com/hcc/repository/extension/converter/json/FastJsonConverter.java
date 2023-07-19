package com.hcc.repository.extension.converter.json;

import com.alibaba.fastjson.JSON;

/**
 * FastJsonConverter
 *
 * @author hushengjun
 * @date 2023/7/19
 */
public class FastJsonConverter implements JsonConverter<Object> {

    private final Class<?> targetClass;

    public FastJsonConverter(Class<?> targetClass) {
        this.targetClass = targetClass;
    }

    @Override
    public String toJson(Object attribute) {
        return JSON.toJSONString(attribute);
    }

    @Override
    public Object parseJson(String json) {
        return JSON.parseObject(json, targetClass);
    }

}
