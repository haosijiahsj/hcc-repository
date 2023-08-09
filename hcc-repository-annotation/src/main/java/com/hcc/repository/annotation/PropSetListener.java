package com.hcc.repository.annotation;

/**
 * 实体字段反射set值前的监听器<br/>
 * 此方法在返回数据实体前调用
 * @author hushengjun
 * @date 2023/4/25
 */
public interface PropSetListener {

    /**
     * 满足条件的才会执行onPropSet方法
     * @param entity
     * @param value
     * @param propName
     * @param columnName
     * @return
     */
    default boolean test(Object entity, Object value, String propName, String columnName) {
        return true;
    }

    /**
     * 实体属性值设置后回调方法
     * @param entity
     * @param value
     * @param propName
     * @param columnName
     * @return
     */
    default Object onPropSet(Object entity, Object value, String propName, String columnName) {
        return value;
    }

}
