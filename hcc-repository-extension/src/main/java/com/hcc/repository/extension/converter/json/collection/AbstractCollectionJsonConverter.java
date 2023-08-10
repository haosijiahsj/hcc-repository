package com.hcc.repository.extension.converter.json.collection;

import com.hcc.repository.core.utils.StrUtils;

import java.lang.reflect.Field;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.util.Collection;
import java.util.List;

/**
 * AbstractCollectionJsonConverter
 *
 * @author hushengjun
 * @date 2023/8/9
 */
public abstract class AbstractCollectionJsonConverter<T> implements CollectionJsonConverter<T> {

    private Class<?> targetClass;
    private Class<T> genericsClass;

    public AbstractCollectionJsonConverter(Field field) {
        targetClass = field.getType();
        Type genericType = field.getGenericType();
        if (genericType instanceof ParameterizedType) {
            ParameterizedType parameterizedType = (ParameterizedType) genericType;
            Type[] actualTypeArguments = parameterizedType.getActualTypeArguments();
            if (actualTypeArguments.length > 0) {
                genericsClass = (Class<T>) actualTypeArguments[0];
            }
        }
        if (genericsClass == null) {
            throw new IllegalArgumentException(StrUtils.format("字段：{0}未获取到泛型类型", getTargetClass().getName()));
        }
    }

    @Override
    public Class<?> getTargetClass() {
        return targetClass;
    }

    @Override
    public abstract String toJson(Collection<T> attribute);

    public Class<T> getGenericsClass() {
        return genericsClass;
    }

    @Override
    public abstract List<T> parseJsonToList(String json);

}
