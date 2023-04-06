package com.hcc.repository.extension.conditions;

import com.hcc.repository.core.mapper.BaseMapper;
import com.hcc.repository.extension.conditions.query.DefaultChainQueryCondition;
import com.hcc.repository.extension.conditions.query.LambdaChainQueryCondition;
import com.hcc.repository.extension.conditions.update.DefaultChainUpdateCondition;
import com.hcc.repository.extension.conditions.update.LambdaChainUpdateCondition;

import java.io.Serializable;

/**
 * ConditionBuilder
 *
 * @author hushengjun
 * @date 2023/3/7
 */
public final class ChainConditions {

    private ChainConditions() {}

    public static <T, ID extends Serializable> DefaultChainQueryCondition<T, ID> defaultQuery(BaseMapper<T, ID> baseMapper) {
        return new DefaultChainQueryCondition<>(baseMapper);
    }

    public static <T, ID extends Serializable> LambdaChainQueryCondition<T, ID> lambdaQuery(BaseMapper<T, ID> baseMapper) {
        return new LambdaChainQueryCondition<>(baseMapper);
    }

    public static <T, ID extends Serializable> DefaultChainUpdateCondition<T, ID> defaultUpdate(BaseMapper<T, ID> baseMapper) {
        return new DefaultChainUpdateCondition<>(baseMapper);
    }

    public static <T, ID extends Serializable> LambdaChainUpdateCondition<T, ID> lambdaUpdate(BaseMapper<T, ID> baseMapper) {
        return new LambdaChainUpdateCondition<>(baseMapper);
    }

}
