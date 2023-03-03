package com.hcc.repository.annotation;

/**
 * IConverter
 *
 * @author hushengjun
 * @date 2023/3/3
 */
public interface IConverter<X, Y> {

    /**
     * 转换到数据库列
     * @param attribute
     * @return
     */
    Y convertToColumn(X attribute);

    /**
     * 转换到实体属性
     * @param column
     * @return
     */
    X convertToAttribute(Y column);

}
