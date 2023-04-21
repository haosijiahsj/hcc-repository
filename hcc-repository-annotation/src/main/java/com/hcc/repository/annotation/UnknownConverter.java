package com.hcc.repository.annotation;

/**
 * 默认的转换器
 *
 * @author hushengjun
 * @date 2023/3/3
 */
public class UnknownConverter implements IConverter<Object, Object> {

    @Override
    public Object convertToColumn(Object attributeValue) {
        throw new UnsupportedOperationException();
    }

    @Override
    public Object convertToAttribute(Object columnValue) {
        throw new UnsupportedOperationException();
    }

}
