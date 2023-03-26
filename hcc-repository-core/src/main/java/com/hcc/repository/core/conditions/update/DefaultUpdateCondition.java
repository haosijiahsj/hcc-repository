package com.hcc.repository.core.conditions.update;

import com.hcc.repository.core.conditions.SegmentContainer;

import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * UpdateConditions
 *
 * @author hushengjun
 * @date 2023/3/3
 */
public class DefaultUpdateCondition<T> extends AbstractUpdateCondition<T, String, DefaultUpdateCondition<T>> {

    public DefaultUpdateCondition() {
        super.init();
    }

    public DefaultUpdateCondition(Class<T> entityClass) {
        this();
        super.setEntityClass(entityClass);
    }

    public DefaultUpdateCondition(Class<T> entityClass, SegmentContainer segmentContainer, Map<String, Object> columnValuePairs, AtomicInteger pos) {
        super.entityClass = entityClass;
        super.segmentContainer = segmentContainer;
        super.columnValuePairs = columnValuePairs;
        super.pos = pos;
    }

    @Override
    protected DefaultUpdateCondition<T> newInstance() {
        return new DefaultUpdateCondition<>(entityClass, new SegmentContainer(), columnValuePairs, pos);
    }

}
