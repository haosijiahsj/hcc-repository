package com.hcc.repository.extension.converter.json.collection;

import com.google.gson.Gson;

import java.lang.reflect.Field;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.util.Collection;
import java.util.List;

/**
 * CollectionGsonConverter
 *
 * @author hushengjun
 * @date 2023/7/19
 */
public class CollectionGsonConverter<T> extends AbstractCollectionJsonConverter<T> {

    private static final Gson GSON = new Gson();

    public CollectionGsonConverter(Field field) {
        super(field);
    }

    @Override
    public String toJson(Collection<T> attribute) {
        return GSON.toJson(attribute);
    }

    @Override
    public List<T> parseJsonToList(String json) {
        return GSON.fromJson(json, new ListParameterizedType(getGenericsClass()));
    }

    private static class ListParameterizedType implements ParameterizedType {
        private final Class<?> clazz;

        public ListParameterizedType(Class<?> clazz) {
            this.clazz = clazz;
        }

        @Override
        public Type[] getActualTypeArguments() {
            return new Type[]{clazz};
        }

        @Override
        public Type getRawType() {
            return List.class;
        }

        @Override
        public Type getOwnerType() {
            return null;
        }
    }

}
