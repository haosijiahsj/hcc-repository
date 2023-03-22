package com.hcc.repository.core.conditions.insert;

/**
 * DefaultInsertCondition
 *
 * @author hushengjun
 * @date 2023/3/21
 */
public class DefaultInsertCondition<T> extends AbstractInsertCondition<T, String> {

    public DefaultInsertCondition(T entity) {
        this.init(entity);
    }

}
