package com.hcc.repository.core.condition;

import com.hcc.repository.core.condition.interfaces.SFunction;

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

        // TODO 这里要添加缓存逻辑
        String methodName = column.getImplMethodName();

        return this.resolveMethodName(methodName);
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
