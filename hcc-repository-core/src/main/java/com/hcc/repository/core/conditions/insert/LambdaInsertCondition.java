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
public class LambdaInsertCondition<T> extends AbstractInsertCondition<T, SFunction<T, ?>> {

    public LambdaInsertCondition(T entity) {
        this.init(entity);
    }

    @Override
    public AbstractInsertCondition<T, SFunction<T, ?>> value(boolean condition, SFunction<T, ?> column, Object val) {
        if (condition) {
            String columnName = this.getColumnName(column);
            sqlColumns.add(columnName);
            sqlValues.add(":" + columnName);
            columnValuePairs.put(columnName, val);
        }
        return this;
    }

    protected String getColumnName(SFunction<T, ?> column) {
        if (column == null) {
            throw new NullPointerException();
        }

        String methodName = column.getImplMethodName();
        String fieldName = this.resolveMethodName(methodName);
        TableColumnInfo tableColumnInfo = TableInfoHelper.getColumnInfoByClassAndFieldName(getEntityClass(), fieldName);
        if (tableColumnInfo == null) {
            throw new IllegalArgumentException("该字段未映射");
        }

        return tableColumnInfo.getColumnName();
    }

    private String resolveMethodName(String methodName) {
        if (methodName == null) {
            return null;
        }

        String columnName;
        if (methodName.startsWith("is")) {
            columnName = this.methodToField(methodName, "is");
        } else if (methodName.startsWith("get")) {
            columnName = this.methodToField(methodName, "get");
        } else if (methodName.startsWith("set")) {
            columnName = this.methodToField(methodName, "set");
        } else {
            columnName = methodName;
        }

        return columnName;
    }

    private String methodToField(String methodName, String prefix) {
        int index = methodName.indexOf(prefix) + prefix.length();
        char c = methodName.charAt(index);
        return String.valueOf(c).toLowerCase() + methodName.substring(index + 1);
    }

}
