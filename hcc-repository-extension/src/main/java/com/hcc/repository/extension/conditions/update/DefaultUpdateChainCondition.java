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
public class DefaultUpdateChainCondition<T, ID extends Serializable>
        extends AbstractChainCondition<T, String, DefaultUpdateChainCondition<T, ID>, DefaultUpdateCondition<T>>
        implements ChainUpdate<T, ID>, SetClause<DefaultUpdateChainCondition<T, ID>, String> {

    private final BaseMapper<T, ID> baseMapper;

    public DefaultUpdateChainCondition(BaseMapper<T, ID> baseMapper) {
        this.baseMapper = baseMapper;
        super.ch = new DefaultUpdateCondition<>();
    }

    @Override
    public BaseMapper<T, ID> getBaseMapper() {
        return baseMapper;
    }

    @Override
    public DefaultUpdateChainCondition<T, ID> set(boolean condition, String column, Object val) {
        ch.set(condition, column, val);
        return typeThis;
    }

    @Override
    public DefaultUpdateChainCondition<T, ID> setSql(boolean condition, String setSql) {
        ch.setSql(condition, setSql);
        return typeThis;
    }

}
