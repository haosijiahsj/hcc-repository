package com.hcc.repository.extension.conditions.update;

import com.hcc.repository.core.conditions.interfaces.SetClause;
import com.hcc.repository.core.conditions.update.DefaultUpdateCondition;
import com.hcc.repository.core.mapper.BaseMapper;
import com.hcc.repository.extension.conditions.AbstractChainCondition;

import java.io.Serializable;

/**
 * DefaultChainUpdateCondition
 *
 * @author hushengjun
 * @date 2023/4/3
 */
public class DefaultChainUpdateCondition<T, ID extends Serializable>
        extends AbstractChainCondition<T, String, DefaultChainUpdateCondition<T, ID>, DefaultUpdateCondition<T>>
        implements ChainUpdate<T, ID>, SetClause<DefaultChainUpdateCondition<T, ID>, String> {

    private final BaseMapper<T, ID> baseMapper;

    public DefaultChainUpdateCondition(BaseMapper<T, ID> baseMapper) {
        this.baseMapper = baseMapper;
    }

    @Override
    public BaseMapper<T, ID> getBaseMapper() {
        return baseMapper;
    }

    @Override
    public DefaultChainUpdateCondition<T, ID> set(boolean condition, String column, Object val) {
        ch.set(condition, column, val);
        return typeThis;
    }

    @Override
    public DefaultChainUpdateCondition<T, ID> setSql(boolean condition, String setSql) {
        ch.setSql(condition, setSql);
        return typeThis;
    }

}
