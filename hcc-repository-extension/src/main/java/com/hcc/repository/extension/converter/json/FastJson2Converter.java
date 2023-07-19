package com.hcc.repository.extension.converter.json;

import com.alibaba.fastjson2.JSON;

/**
 * FastJson2Converter
 *
 * @author hushengjun
 * @date 2023/7/19
 */
public class FastJson2Converter implements JsonConverter<Object> {

    private final Class<?> targetClass;

    public FastJson2Converter(Class<?> targetClass) {
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
