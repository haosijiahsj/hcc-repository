package com.hcc.repository.extension.generator.id;

import com.hcc.repository.annotation.IdGenerator;

import java.util.UUID;

/**
 * uuid的id生成器
 *
 * @author hushengjun
 * @date 2023/4/4
 */
public class UuidIdGenerator implements IdGenerator<String> {

    @Override
    public String nextId() {
        return UUID.randomUUID().toString().replaceAll("-", "");
    }

}
