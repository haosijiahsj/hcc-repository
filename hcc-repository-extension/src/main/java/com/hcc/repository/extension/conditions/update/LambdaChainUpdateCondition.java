package com.hcc.repository.extension.conditions.update;

import com.hcc.repository.core.conditions.interfaces.SFunction;
import com.hcc.repository.core.conditions.interfaces.SetClause;
import com.hcc.repository.core.conditions.update.LambdaUpdateCondition;
import com.hcc.repository.core.mapper.BaseMapper;
import com.hcc.repository.extension.conditions.AbstractChainCondition;

import java.io.Serializable;

/**
 * LambdaChainUpdateCondition
 *
 * @author hushengjun
 * @date 2023/4/3
 */
public class LambdaChainUpdateCondition<T, ID extends Serializable>
        extends AbstractChainCondition<T, SFunction<T, ?>, LambdaChainUpdateCondition<T, ID>, LambdaUpdateCondition<T>>
        implements ChainUpdate<T, ID>, SetClause<LambdaChainUpdateCondition<T, ID>, SFunction<T, ?>> {

    private final BaseMapper<T, ID> baseMapper;

    public LambdaChainUpdateCondition(BaseMapper<T, ID> baseMapper) {
        this.baseMapper = baseMapper;
    }

    @Override
    public LambdaChainUpdateCondition<T, ID> set(boolean condition, SFunction<T, ?> column, Object val) {
        ch.set(condition, column, val);
        return typeThis;
    }

    @Override
    public LambdaChainUpdateCondition<T, ID> setSql(boolean condition, String setSql) {
        ch.setSql(condition, setSql);
        return typeThis;
    }

    @Override
    public BaseMapper<T, ID> getBaseMapper() {
        return baseMapper;
    }

}
