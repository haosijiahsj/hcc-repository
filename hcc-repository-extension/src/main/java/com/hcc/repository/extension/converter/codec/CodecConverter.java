package com.hcc.repository.extension.converter.codec;

import com.hcc.repository.extension.converter.StringConverter;

/**
 * 编解码converter
 *
 * @author hushengjun
 * @date 2023/4/25
 */
public interface CodecConverter extends StringConverter {

    @Override
    default String convertToColumn(String attribute) {
        if (attribute == null) {
            return null;
        }

        return encode(attribute);
    }

    @Override
    default String convertToAttribute(String column) {
        if (column == null) {
            return null;
        }

        return decode(column);
    }

    /**
     * 编码方法
     * @param originalStr
     * @return
     */
    default String encode(String originalStr) {
        return originalStr;
    }

    /**
     * 解码方法
     * @param encodedStr
     * @return
     */
    default String decode(String encodedStr) {
        return encodedStr;
    }

}
