package com.hcc.repository.extension.converter.json.collection;

import com.alibaba.fastjson.JSON;

import java.lang.reflect.Field;
import java.util.Collection;
import java.util.List;

/**
 * CollectionFastJsonConverter
 *
 * @author hushengjun
 * @date 2023/7/19
 */
public class CollectionFastJsonConverter<T> extends AbstractCollectionJsonConverter<T> {

    public CollectionFastJsonConverter(Field field) {
        super(field);
    }

    @Override
    public String toJson(Collection<T> attribute) {
        return JSON.toJSONString(attribute);
    }

    @Override
    public List<T> parseJsonToList(String json) {
        return JSON.parseArray(json, getGenericsClass());
    }

}
