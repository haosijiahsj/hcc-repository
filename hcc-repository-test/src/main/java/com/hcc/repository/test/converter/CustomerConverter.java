package com.hcc.repository.test.converter;


import com.hcc.repository.extension.converter.AbstractEnumConverter;
import com.hcc.repository.test.enums.SexEnum;

/**
 * CustomerConverter
 *
 * @author hushengjun
 * @date 2023/3/24
 */
public class CustomerConverter extends AbstractEnumConverter<SexEnum, Integer> {

    @Override
    public Integer toColumn(SexEnum attribute) {
        return attribute.getCode();
    }

    @Override
    public SexEnum toEnum(Integer column) {
        for (SexEnum sexEnum : SexEnum.values()) {
            if (sexEnum.getCode() == column) {
                return sexEnum;
            }
        }
        return null;
    }

}
