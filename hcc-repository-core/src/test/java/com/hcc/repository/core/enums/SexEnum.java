package com.hcc.repository.core.enums;

/**
 * SexEnum
 *
 * @author hushengjun
 * @date 2023/3/24
 */
public enum SexEnum {

    MALE(1),
    FEMALE(0);

    private int code;

    SexEnum(int code) {
        this.code = code;
    }

    public int getCode() {
        return code;
    }
}
