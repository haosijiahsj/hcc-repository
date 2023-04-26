package com.hcc.repository.annotation;

/**
 * IdType
 *
 * @author hushengjun
 * @date 2023/3/3
 */
public enum IdType {

    AUTO("数据库自增"),
    SPECIFY("自行传入"),
    GENERATE("IdGenerator生成");

    final String desc;

    IdType(String desc) {
        this.desc = desc;
    }

}
