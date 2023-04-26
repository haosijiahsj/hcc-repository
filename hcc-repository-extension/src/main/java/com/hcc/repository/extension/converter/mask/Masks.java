package com.hcc.repository.extension.converter.mask;

/**
 * 默认的一些mask工具
 *
 * @author hushengjun
 * @date 2023/4/25
 */
public interface Masks {

    /**
     * 11位手机号，前3后4，中间*
     */
    class MobileMaskConverter implements MaskConverter {
        @Override
        public String mask(String data) {
            if (data.length() != 11) {
                return data;
            }
            return mask(data, 3, 4, 4);
        }
    }

    /**
     * 身份证号，前3后4，中间*
     */
    class IdCardNumMaskConverter implements MaskConverter {
        @Override
        public String mask(String data) {
            if (data.length() < 15) {
                return data;
            }
            return mask(data, 3, 4, data.length() - 7);
        }
    }

}
