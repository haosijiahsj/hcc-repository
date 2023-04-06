package com.hcc.repository.extension.conditions.query;

import com.hcc.repository.core.conditions.ICondition;
import com.hcc.repository.core.conditions.interfaces.SelectClause;
import com.hcc.repository.core.conditions.query.DefaultQueryCondition;
import com.hcc.repository.core.mapper.BaseMapper;
import com.hcc.repository.core.metadata.TableColumnInfo;
import com.hcc.repository.extension.conditions.AbstractChainCondition;

import java.io.Serializable;
import java.util.function.Predicate;

/**
 * DefaultChainQueryCondition
 *
 * @author hushengjun
 * @date 2023/4/3
 */
public class DefaultChainQueryCondition<T, ID extends Serializable>
        extends AbstractChainCondition<T, String, DefaultChainQueryCondition<T, ID>, DefaultQueryCondition<T>>
        implements ChainQuery<T, ID>, SelectClause<DefaultChainQueryCondition<T, ID>, T, String> {

    private final BaseMapper<T, ID> baseMapper;

    public DefaultChainQueryCondition(BaseMapper<T, ID> baseMapper) {
        this.baseMapper = baseMapper;
        super.ch = new DefaultQueryCondition<>();
    }

    @Override
    public BaseMapper<T, ID> getBaseMapper() {
        return baseMapper;
    }

    @Override
    public DefaultChainQueryCondition<T, ID> select(String... columns) {
        ch.select(columns);
        return typeThis;
    }

    @Override
    public DefaultChainQueryCondition<T, ID> select(Class<T> entityClass, Predicate<TableColumnInfo> predicate) {
        ch.select(entityClass, predicate);
        return typeThis;
    }

}
