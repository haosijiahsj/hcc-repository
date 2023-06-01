package com.hcc.repository.extension.conditions;

import com.hcc.repository.core.mapper.BaseMapper;
import com.hcc.repository.extension.conditions.nativesql.NativeSqlChainCondition;
import com.hcc.repository.extension.conditions.query.DefaultQueryChainCondition;
import com.hcc.repository.extension.conditions.query.LambdaQueryChainCondition;
import com.hcc.repository.extension.conditions.update.DefaultUpdateChainCondition;
import com.hcc.repository.extension.conditions.update.LambdaUpdateChainCondition;

import java.io.Serializable;

/**
 * ConditionBuilder
 *
 * @author hushengjun
 * @date 2023/3/7
 */
public final class ChainConditions {

    private ChainConditions() {}

    public static <T, ID extends Serializable> DefaultQueryChainCondition<T, ID> defaultQuery(BaseMapper<T, ID> baseMapper) {
        return new DefaultQueryChainCondition<>(baseMapper);
    }

    public static <T, ID extends Serializable> LambdaQueryChainCondition<T, ID> lambdaQuery(BaseMapper<T, ID> baseMapper) {
        return new LambdaQueryChainCondition<>(baseMapper);
    }

    public static <T, ID extends Serializable> DefaultUpdateChainCondition<T, ID> defaultUpdate(BaseMapper<T, ID> baseMapper) {
        return new DefaultUpdateChainCondition<>(baseMapper);
    }

    public static <T, ID extends Serializable> LambdaUpdateChainCondition<T, ID> lambdaUpdate(BaseMapper<T, ID> baseMapper) {
        return new LambdaUpdateChainCondition<>(baseMapper);
    }

    public static <T, ID extends Serializable> NativeSqlChainCondition<T, ID> nativeSql(BaseMapper<T, ID> baseMapper) {
        return new NativeSqlChainCondition<>(baseMapper);
    }

}
