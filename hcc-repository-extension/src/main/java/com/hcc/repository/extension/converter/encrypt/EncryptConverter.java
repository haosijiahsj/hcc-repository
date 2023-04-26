package com.hcc.repository.extension.converter.encrypt;

import com.hcc.repository.extension.converter.StringConverter;

/**
 * 加解密converter
 *
 * @author hushengjun
 * @date 2023/4/25
 */
public interface EncryptConverter extends StringConverter {

    @Override
    default String convertToColumn(String attribute) {
        if (attribute == null) {
            return null;
        }

        return encrypt(attribute);
    }

    @Override
    default String convertToAttribute(String column) {
        if (column == null) {
            return null;
        }

        return decrypt(column);
    }

    /**
     * 加密方法
     * @param originalText
     * @return
     */
    String encrypt(String originalText);

    /**
     * 解密方法
     * @param ciphertext
     * @return
     */
    String decrypt(String ciphertext);

}
