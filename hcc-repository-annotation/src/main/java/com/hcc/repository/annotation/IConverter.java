package com.hcc.repository.annotation;

/**
 * IConverter<br/>
 * 提供字段class与字段的注入，实现时提供对应构造方法即可，多个会按以下顺序使用构造方法<br/>
 * public CustomerConverter(Class<?> clazz) {}<br/>
 * public CustomerConverter(Field field) {}<br/>
 * public CustomerConverter() {}<br/>
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
