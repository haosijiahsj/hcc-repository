package com.hcc.repository.core.conditions.query;

import com.hcc.repository.core.conditions.SegmentContainer;

import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * QueryConditions
 *
 * @author hushengjun
 * @date 2023/3/3
 */
public class DefaultQueryCondition<T> extends AbstractQueryCondition<T, String, DefaultQueryCondition<T>> {

    public DefaultQueryCondition() {
        super.init();
    }

    public DefaultQueryCondition(Class<T> entityClass) {
        this();
        super.setEntityClass(entityClass);
    }

    public DefaultQueryCondition(Class<T> entityClass, SegmentContainer segmentContainer, Map<String, Object> columnValuePairs, AtomicInteger pos) {
        super.entityClass = entityClass;
        super.segmentContainer = segmentContainer;
        super.columnValuePairs = columnValuePairs;
        super.pos = pos;
    }

    @Override
    protected DefaultQueryCondition<T> newInstance() {
        return new DefaultQueryCondition<>(entityClass, new SegmentContainer(), columnValuePairs, pos);
    }

    /**
     * 同时支持lambda的写法
     * @return
     */
    public LambdaQueryCondition<T> forLambda() {
        return new LambdaQueryCondition<>(entityClass, selectColumns, segmentContainer, columnValuePairs, pos);
    }

}
