package com.hcc.repository.annotation;

/**
 * IConverter
 *
 * @author hushengjun
 * @date 2023/3/3
 */
public interface IConverter<ATTR, COL> {

    /**
     * 转换到数据库列
     * @param attribute
     * @return
     */
    default COL convertToColumn(ATTR attribute) {
        throw new UnsupportedOperationException();
    }

    /**
     * 转换到实体属性
     * @param column
     * @return
     */
    default ATTR convertToAttribute(COL column) {
        throw new UnsupportedOperationException();
    }

}
