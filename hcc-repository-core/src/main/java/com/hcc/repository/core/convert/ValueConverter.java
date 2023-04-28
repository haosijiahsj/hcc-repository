package com.hcc.repository.core.convert;

/**
 * IConverter
 *
 * @author hushengjun
 * @date 2023/3/24
 */
@FunctionalInterface
public interface ValueConverter<T> {

    /**
     * 转换器
     * @param value
     * @param targetClass
     * @return
     */
    T convert(Object value, Class<?> targetClass);

}
