package com.hcc.repository.annotation;

import java.io.Serializable;

/**
 * DefaultIdGenerator
 *
 * @author hushengjun
 * @date 2023/3/5
 */
public class UnknownIdGenerator implements IdGenerator<Serializable> {

    @Override
    public Serializable nextId() {
        throw new UnsupportedOperationException("请实现IdGenerator");
    }

}
