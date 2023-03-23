package com.hcc.repository.core.conditions;

import com.hcc.repository.core.conditions.interfaces.SFunction;
import com.hcc.repository.core.metadata.TableColumnInfo;
import com.hcc.repository.core.metadata.TableInfoHelper;

/**
 * AbstractLambdaCondition
 *
 * @author hushengjun
 * @date 2023/3/14
 */
public abstract class AbstractLambdaCondition<T, C extends AbstractCondition<T, SFunction<T, ?>, C>> extends AbstractCondition<T, SFunction<T, ?>, C> {

    @Override
    protected String getColumnName(SFunction<T, ?> column) {
        if (column == null) {
            throw new NullPointerException();
        }

        String methodName = column.getImplMethodName();
        String fieldName = this.resolveMethodName(methodName);
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
