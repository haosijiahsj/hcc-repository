package com.hcc.repository.extension.converter;

import com.hcc.repository.annotation.IConverter;

/**
 * 字符串类型的converter
 *
 * @author hushengjun
 * @date 2023/4/23
 */
public interface StringConverter extends IConverter<String, String> {

    @Override
    default String convertToColumn(String attribute) {
        return attribute;
    }

    @Override
    default String convertToAttribute(String column) {
        return column;
    }

}
