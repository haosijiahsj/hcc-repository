package com.hcc.repository.extension.converter.json;

import com.hcc.repository.annotation.IConverter;
import com.hcc.repository.core.utils.StrUtils;

/**
 * json转换器
 *
 * @author hushengjun
 * @date 2023/4/4
 */
public interface JsonConverter<T> extends IConverter<T, String> {

    @Override
    default String convertToColumn(T attribute) {
        if (attribute == null) {
            return null;
        }

        return toJson(attribute);
    }

    @Override
    default T convertToAttribute(String column) {
        if (StrUtils.isEmpty(column)) {
            return null;
        }

        return parseJson(column);
    }

    /**
     * 转换到json
     * @param attribute
     * @return
     */
    String toJson(T attribute);

    /**
     * 转换到对象
     * @param json
     * @return
     */
    T parseJson(String json);

}
