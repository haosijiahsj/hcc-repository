package com.hcc.repository.extension.converter.codec;

import org.apache.commons.codec.binary.Base64;

/**
 * Base64加解密
 *
 * @author hushengjun
 * @date 2023/8/2
 */
public class Base64CodecConverter implements CodecConverter {

    @Override
    public String encode(String originalStr) {
        return new String(Base64.encodeBase64(originalStr.getBytes()));
    }

    @Override
    public String decode(String encodedStr) {
        return new String(Base64.decodeBase64(encodedStr.getBytes()));
    }

}
