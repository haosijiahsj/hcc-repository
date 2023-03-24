package com.hcc.repository.core.converter;

import com.hcc.repository.annotation.IConverter;
import com.hcc.repository.core.enums.SexEnum;

/**
 * CustomerConverter
 *
 * @author hushengjun
 * @date 2023/3/24
 */
public class CustomerConverter implements IConverter<SexEnum, Integer> {

    @Override
    public Integer convertToColumn(SexEnum attribute) {
        return attribute.getCode();
    }

    @Override
    public SexEnum convertToAttribute(Integer column) {
        for (SexEnum sexEnum : SexEnum.values()) {
            if (sexEnum.getCode() == column) {
                return sexEnum;
            }
        }
        return null;
    }

}
