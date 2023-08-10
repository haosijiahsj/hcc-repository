package com.hcc.repository.extension.converter.mask;

import com.hcc.repository.extension.converter.StringConverter;

/**
 * 脱敏转换器，要求字段为String
 *
 * @author hushengjun
 * @date 2023/4/23
 */
public interface MaskConverter extends StringConverter {

    String MASK_PLACEHOLDER = "*";

    @Override
    default String convertToColumn(String attribute) {
        return attribute;
    }

    @Override
    default String convertToAttribute(String column) {
        if (column == null) {
            return null;
        }

        if (MaskContextHolder.get()) {
            return column;
        }

        return this.mask(column);
    }

    /**
     * 脱敏占位符
     * @return
     */
    default String maskPlaceholder() {
        return MASK_PLACEHOLDER;
    }

    /**
     * 脱敏工具方法
     * @param original
     * @param prefixLen
     * @param maskLen
     * @param suffixLen
     * @param maskPlaceholder
     * @return
     */
    default String mask(String original, int prefixLen, int maskLen, int suffixLen, String maskPlaceholder) {
        StringBuilder s = new StringBuilder();
        s.append(original, 0, prefixLen);
        for (int i = 0; i < maskLen; i++) {
            s.append(maskPlaceholder);
        }
        s.append(original.substring(original.length() - suffixLen));

        return s.toString();
    }

    /**
     * 使用*号脱敏
     * @param original
     * @param prefixLen
     * @param maskLen
     * @param suffixLen
     * @return
     */
    default String mask(String original, int prefixLen, int maskLen, int suffixLen) {
        return mask(original, prefixLen, maskLen, suffixLen, maskPlaceholder());
    }

    /**
     * 脱敏方法
     * @param data
     * @return
     */
    String mask(String data);

}
