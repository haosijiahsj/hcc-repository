package com.hcc.repository.extension.converter.codec;

import org.apache.commons.codec.digest.DigestUtils;

/**
 * SHA编码器，解码未处理
 *
 * @author hushengjun
 * @date 2023/8/2
 */
public class SHACodecConverter implements CodecConverter {

    @Override
    public String encode(String originalStr) {
        return DigestUtils.sha256Hex(originalStr);
    }

}
