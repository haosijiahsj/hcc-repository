package com.hcc.repository.annotation;

/**
 * IdentifierGenerator
 *
 * @author shengjun.hu
 * @date 2021/6/11
 */
public interface IdGenerator<T> {

    T nextId();

}
