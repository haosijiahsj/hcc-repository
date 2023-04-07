package com.hcc.repository.test.domain.enums;

import com.hcc.repository.annotation.IEnum;

/**
 * ProductStatusEnum
 *
 * @author hushengjun
 * @date 2023/4/7
 */
public enum ProductStatusEnum implements IEnum<Integer> {
    SHELF(1, "上架"),
    UN_SHELF(0, "下架"),
    ;

    private Integer code;
    private String name;

    ProductStatusEnum(Integer code, String name) {
        this.code = code;
        this.name = name;
    }

    @Override
    public Integer getValue() {
        return code;
    }

}
