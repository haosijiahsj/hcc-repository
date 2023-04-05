package com.hcc.repository.core.conditions.insert;

import com.hcc.repository.core.conditions.interfaces.SFunction;
import com.hcc.repository.core.constants.StrPool;
import com.hcc.repository.core.metadata.TableColumnInfo;
import com.hcc.repository.core.metadata.TableInfoHelper;

/**
 * DefaultInsertCondition
 *
 * @author hushengjun
 * @date 2023/3/21
 */
public class LambdaInsertCondition<T> extends AbstractInsertCondition<T, SFunction<T, ?>> {

    public LambdaInsertCondition(T entity) {
        this.init(entity);
    }

    @Override
    public AbstractInsertCondition<T, SFunction<T, ?>> value(boolean condition, SFunction<T, ?> column, Object val) {
        if (condition) {
            String columnName = this.getColumnName(column);
            sqlColumns.add(columnName);
            sqlValues.add(StrPool.COLON + columnName);
            columnValuePairs.put(columnName, val);
        }
        return this;
    }

    protected String getColumnName(SFunction<T, ?> column) {
        if (column == null) {
            throw new NullPointerException();
        }

        String fieldName = column.getFieldName();
        TableColumnInfo tableColumnInfo = TableInfoHelper.getColumnInfoByClassAndFieldName(getEntityClass(), fieldName);
        if (tableColumnInfo == null) {
            throw new IllegalArgumentException("该字段未映射");
        }

        return tableColumnInfo.getColumnName();
    }


}
