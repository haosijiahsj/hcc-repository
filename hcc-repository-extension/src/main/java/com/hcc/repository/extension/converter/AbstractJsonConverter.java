package com.hcc.repository.extension.converter;

import com.hcc.repository.annotation.IConverter;
import com.hcc.repository.core.utils.StrUtils;

/**
 * 抽象的json转换器
 *
 * @author hushengjun
 * @date 2023/4/4
 */
public abstract class AbstractJsonConverter<T> implements IConverter<T, String> {

    @Override
    public String convertToColumn(T attribute) {
        if (attribute == null) {
            return null;
        }

        return toJson(attribute);
    }

    @Override
    public T convertToAttribute(String column) {
        if (StrUtils.isEmpty(column)) {
            return null;
        }

        return parseJson(column);
    }

    public abstract String toJson(T attribute);

    public abstract T parseJson(String json);

}
