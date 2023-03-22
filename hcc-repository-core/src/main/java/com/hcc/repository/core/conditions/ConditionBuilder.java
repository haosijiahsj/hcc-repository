package com.hcc.repository.core.conditions;

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
public final class ConditionBuilder {

    private ConditionBuilder() {}

    public static <T> DefaultQueryCondition<T> query() {
        return new DefaultQueryCondition<>();
    }

    public static <T> DefaultQueryCondition<T> query(Class<T> clazz) {
        return new DefaultQueryCondition<>(clazz);
    }

    public static <T> LambdaQueryCondition<T> lambdaQuery() {
        return new LambdaQueryCondition<>();
    }

    public static <T> LambdaQueryCondition<T> lambdaQuery(Class<T> clazz) {
        return new LambdaQueryCondition<>(clazz);
    }

    public static <T> DefaultUpdateCondition<T> update() {
        return new DefaultUpdateCondition<>();
    }

    public static <T> DefaultUpdateCondition<T> update(Class<T> clazz) {
        return new DefaultUpdateCondition<>(clazz);
    }

    public static <T> LambdaUpdateCondition<T> lambdaUpdate() {
        return new LambdaUpdateCondition<>();
    }

    public static <T> LambdaUpdateCondition<T> lambdaUpdate(Class<T> clazz) {
        return new LambdaUpdateCondition<>(clazz);
    }

}
