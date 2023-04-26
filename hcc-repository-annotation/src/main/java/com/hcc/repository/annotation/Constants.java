package com.hcc.repository.annotation;

import java.io.Serializable;

/**
 * Constants
 *
 * @author hushengjun
 * @date 2023/4/25
 */
public interface Constants {

    /**
     * 默认的converter
     */
    class UnknownConverter implements IConverter<Object, Object> {}

    /**
     * 默认的填充策略
     */
    class UnknownFillStrategy implements AutoFillStrategy {}

    /**
     * 默认的id生成器
     */
    class UnknownIdGenerator implements IdGenerator<Serializable> {
        @Override
        public Serializable nextId() {
            throw new UnsupportedOperationException("请实现IdGenerator");
        }
    }

    /**
     * 默认的属性监听器
     */
    class DefPropSetListener implements PropSetListener {}

}
