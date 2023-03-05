package com.hcc.repository.annotation;

/**
 * DefaultIdGenerator
 *
 * @author hushengjun
 * @date 2023/3/5
 */
public class DefaultIdGenerator implements IdGenerator<Object> {

    @Override
    public Object nextId() {
        throw new UnsupportedOperationException("请实现IdGenerator");
    }

}
