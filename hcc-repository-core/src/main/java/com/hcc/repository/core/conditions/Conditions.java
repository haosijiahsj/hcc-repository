package com.hcc.repository.core.conditions;

import com.hcc.repository.core.conditions.original.OriginalSqlCondition;
import com.hcc.repository.core.conditions.query.DefaultQueryCondition;
import com.hcc.repository.core.conditions.query.LambdaQueryCondition;
import com.hcc.repository.core.conditions.update.DefaultUpdateCondition;
import com.hcc.repository.core.conditions.update.LambdaUpdateCondition;

/**
 * ConditionBuilder
 *
 * @author hushengjun
 * @date 2023/3/7
 */
public final class Conditions {

    private Conditions() {}

    public static <T> DefaultQueryCondition<T> defaultQuery() {
        return new DefaultQueryCondition<>();
    }

    public static <T> DefaultQueryCondition<T> defaultQuery(Class<T> clazz) {
        return new DefaultQueryCondition<>(clazz);
    }

    public static <T> LambdaQueryCondition<T> lambdaQuery() {
        return new LambdaQueryCondition<>();
    }

    public static <T> LambdaQueryCondition<T> lambdaQuery(Class<T> clazz) {
        return new LambdaQueryCondition<>(clazz);
    }

    public static <T> DefaultUpdateCondition<T> defaultUpdate() {
        return new DefaultUpdateCondition<>();
    }

    public static <T> DefaultUpdateCondition<T> defaultUpdate(Class<T> clazz) {
        return new DefaultUpdateCondition<>(clazz);
    }

    public static <T> LambdaUpdateCondition<T> lambdaUpdate() {
        return new LambdaUpdateCondition<>();
    }

    public static <T> LambdaUpdateCondition<T> lambdaUpdate(Class<T> clazz) {
        return new LambdaUpdateCondition<>(clazz);
    }

    public static <T> OriginalSqlCondition<T> originalSql() {
        return new OriginalSqlCondition<>();
    }

}
