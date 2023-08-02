package com.hcc.repository.extension.converter.codec;

import org.apache.commons.codec.digest.DigestUtils;

/**
 * MD5编码器，解码未处理
 *
 * @author hushengjun
 * @date 2023/8/2
 */
public class MD5CodecConverter implements CodecConverter {

    @Override
    public String encode(String originalStr) {
        return DigestUtils.md5Hex(originalStr);
    }

}
