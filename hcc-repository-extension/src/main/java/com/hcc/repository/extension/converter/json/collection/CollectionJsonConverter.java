package com.hcc.repository.extension.converter.json.collection;

import com.hcc.repository.extension.converter.json.JsonConverter;

import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.function.Function;

/**
 * json转换器
 *
 * @author hushengjun
 * @date 2023/4/4
 */
public interface CollectionJsonConverter<T> extends JsonConverter<Collection<T>> {

    default Class<?> getTargetClass() {
        return List.class;
    }

    /**
     * 泛型class
     * @return
     */
    Class<T> getGenericsClass();

    /**
     * 解析json到List列表
     * @return
     */
    List<T> parseJsonToList(String json);

    @Override
    default Collection<T> parseJson(String json) {
        Class<?> targetClass = getTargetClass();
        List<T> result = parseJsonToList(json);
        if (targetClass == null) {
            return result;
        } else if (Set.class.equals(targetClass)) {
            return new HashSet<>(result);
        }

        return result;
    }

}
