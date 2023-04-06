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
    COL convertToColumn(ATTR attribute);

    /**
     * 转换到实体属性
     * @param column
     * @return
     */
    ATTR convertToAttribute(COL column);

}
