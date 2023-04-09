package com.hcc.repository.core.conditions.insert;

import com.hcc.repository.core.conditions.interfaces.SFunction;
import com.hcc.repository.core.metadata.TableColumnInfo;
import com.hcc.repository.core.metadata.TableInfoHelper;

/**
 * DefaultInsertCondition
 *
 * @author hushengjun
 * @date 2023/3/21
 */
public class LambdaInsertCondition<T> extends AbstractInsertCondition<T, SFunction<T, ?>, LambdaInsertCondition<T>> {

    public LambdaInsertCondition() {
        this.init(null);
    }

    public LambdaInsertCondition(Class<T> clazz) {
        this.init(null);
        super.setEntityClass(clazz);
    }

    public LambdaInsertCondition(T entity) {
        this.init(entity);
    }

    @Override
    public LambdaInsertCondition<T> value(boolean condition, SFunction<T, ?> column, Object val) {
        if (condition) {
            String columnName = this.getColumnName(column);
            sqlColumns.add(columnName);
//            sqlValues.add(columnName);
            columnValuePairs.put(columnName, val);
        }
        return this;
    }

    protected String getColumnName(SFunction<T, ?> column) {
        if (column == null) {
            throw new NullPointerException();
        }

        String fieldName = column.getFieldName();
        Class<?> entityClass = getEntityClass();
        if (entityClass == null) {
            // 获取lambda中的class
            entityClass = column.getImplClassType();
        }
        TableColumnInfo tableColumnInfo = TableInfoHelper.getColumnInfoByClassAndFieldName(entityClass, fieldName);
        if (tableColumnInfo == null) {
            throw new IllegalArgumentException("该字段未映射");
        }

        return tableColumnInfo.getColumnName();
    }


}
