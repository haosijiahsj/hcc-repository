package com.hcc.repository.core.conditions.insert;

/**
 * DefaultInsertCondition
 *
 * @author hushengjun
 * @date 2023/3/21
 */
public class DefaultInsertCondition<T> extends AbstractInsertCondition<T, String, DefaultInsertCondition<T>> {

    public DefaultInsertCondition() {
        this.init(null);
    }

    public DefaultInsertCondition(Class<T> clazz) {
        this.init(null);
        super.setEntityClass(clazz);
    }

    public DefaultInsertCondition(T entity) {
        this.init(entity);
    }

}
