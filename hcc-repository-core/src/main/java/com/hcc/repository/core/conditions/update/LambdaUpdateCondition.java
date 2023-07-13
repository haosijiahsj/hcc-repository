package com.hcc.repository.core.conditions.update;

import com.hcc.repository.core.conditions.SegmentContainer;
import com.hcc.repository.core.conditions.interfaces.SFunction;
import com.hcc.repository.core.metadata.TableColumnInfo;
import com.hcc.repository.core.metadata.TableInfoHelper;

import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * LambdaUpdateConditions
 *
 * @author hushengjun
 * @date 2023/3/3
 */
public class LambdaUpdateCondition<T> extends AbstractUpdateCondition<T, SFunction<T, ?>, LambdaUpdateCondition<T>> {

    public LambdaUpdateCondition() {
        super.init();
    }

    public LambdaUpdateCondition(Class<T> entityClass) {
        this();
        super.setEntityClass(entityClass);
    }

    public LambdaUpdateCondition(Class<T> entityClass, SegmentContainer segmentContainer, Map<String, Object> columnValuePairs, AtomicInteger pos) {
        super.entityClass = entityClass;
        super.segmentContainer = segmentContainer;
        super.columnValuePairs = columnValuePairs;
        super.pos = pos;
    }

    @Override
    protected LambdaUpdateCondition<T> newInstance() {
        return new LambdaUpdateCondition<>(entityClass, new SegmentContainer(), columnValuePairs, pos);
    }

    @Override
    @SuppressWarnings("unchecked")
    protected String columnToString(SFunction<T, ?> column) {
        if (column == null) {
            throw new NullPointerException();
        }

        String fieldName = column.getFieldName();
        Class<T> entityClass = getEntityClass();
        if (entityClass == null) {
            // 获取lambda中的class
            entityClass = (Class<T>) column.getImplClassType();
        }
        TableColumnInfo tableColumnInfo = TableInfoHelper.getColumnInfoByClassAndFieldName(entityClass, fieldName);
        if (tableColumnInfo == null) {
            throw new IllegalArgumentException("该字段未映射");
        }

        return tableColumnInfo.getColumnName();
    }

}
