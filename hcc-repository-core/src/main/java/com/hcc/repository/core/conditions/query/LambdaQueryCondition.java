package com.hcc.repository.core.conditions.query;

import com.hcc.repository.core.conditions.SegmentContainer;
import com.hcc.repository.core.conditions.interfaces.SFunction;
import com.hcc.repository.core.metadata.TableColumnInfo;
import com.hcc.repository.core.metadata.TableInfoHelper;

import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * LambdaQueryConditions
 *
 * @author hushengjun
 * @date 2023/3/3
 */
public class LambdaQueryCondition<T> extends AbstractQueryCondition<T, SFunction<T, ?>, LambdaQueryCondition<T>> {

    public LambdaQueryCondition() {
        this((T) null);
    }

    public LambdaQueryCondition(T entity) {
        super.init();
        setEntity(entity);
    }

    public LambdaQueryCondition(Class<T> entityClass) {
        super.init();
        setEntityClass(entityClass);
    }

    public LambdaQueryCondition(Class<T> entityClass, SegmentContainer segmentContainer, Map<String, Object> columnValuePairs, AtomicInteger pos) {
        super.entityClass = entityClass;
        super.segmentContainer = segmentContainer;
        super.columnValuePairs = columnValuePairs;
        super.pos = pos;
    }

    @Override
    protected LambdaQueryCondition<T> newInstance() {
        return new LambdaQueryCondition<>(entityClass, new SegmentContainer(), columnValuePairs, pos);
    }

    @SuppressWarnings("unchecked")
    protected String getColumnName(SFunction<T, ?> column) {
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
