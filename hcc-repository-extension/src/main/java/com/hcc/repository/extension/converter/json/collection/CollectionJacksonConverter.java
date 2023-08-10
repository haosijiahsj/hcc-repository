package com.hcc.repository.extension.converter.json.collection;

import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.SneakyThrows;

import java.lang.reflect.Field;
import java.util.Collection;
import java.util.List;

/**
 * CollectionJacksonConverter
 *
 * @author hushengjun
 * @date 2023/7/19
 */
public class CollectionJacksonConverter<T> extends AbstractCollectionJsonConverter<T> {

    private static final ObjectMapper OBJECT_MAPPER = new ObjectMapper();

    public CollectionJacksonConverter(Field field) {
        super(field);
    }

    @Override
    @SneakyThrows
    public String toJson(Collection<T> attribute) {
        return OBJECT_MAPPER.writeValueAsString(attribute);
    }

    @Override
    @SneakyThrows
    public List<T> parseJsonToList(String json) {
        return OBJECT_MAPPER.readValue(json, OBJECT_MAPPER.getTypeFactory().constructCollectionType(List.class, getGenericsClass()));
    }

}
