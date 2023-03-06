package com.hcc.repository.annotation;

import java.io.Serializable;

/**
 * IdentifierGenerator
 *
 * @author shengjun.hu
 * @date 2021/6/11
 */
@FunctionalInterface
public interface IdGenerator<T extends Serializable> {

    /**
     * 生成主键方法
     * @return
     */
    T nextId();

}
