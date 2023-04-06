package com.hcc.repository.extension.conditions.query;

import com.hcc.repository.core.conditions.interfaces.SFunction;
import com.hcc.repository.core.conditions.interfaces.SelectClause;
import com.hcc.repository.core.conditions.query.LambdaQueryCondition;
import com.hcc.repository.core.mapper.BaseMapper;
import com.hcc.repository.core.metadata.TableColumnInfo;
import com.hcc.repository.extension.conditions.AbstractChainCondition;

import java.io.Serializable;
import java.util.function.Predicate;

/**
 * LambdaChainQueryCondition
 *
 * @author hushengjun
 * @date 2023/4/3
 */
public class LambdaQueryChainCondition<T, ID extends Serializable>
        extends AbstractChainCondition<T, SFunction<T, ?>, LambdaQueryChainCondition<T, ID>, LambdaQueryCondition<T>>
        implements ChainQuery<T, ID>, SelectClause<LambdaQueryChainCondition<T, ID>, T, SFunction<T, ?>> {

    private final BaseMapper<T, ID> baseMapper;

    public LambdaQueryChainCondition(BaseMapper<T, ID> baseMapper) {
        this.baseMapper = baseMapper;
        super.ch = new LambdaQueryCondition<>();
    }

    @Override
    public BaseMapper<T, ID> getBaseMapper() {
        return baseMapper;
    }
    @SafeVarargs
    @Override
    public final LambdaQueryChainCondition<T, ID> select(SFunction<T, ?>... columns) {
        ch.select(columns);
        return typeThis;
    }

    @Override
    public LambdaQueryChainCondition<T, ID> select(Class<T> entityClass, Predicate<TableColumnInfo> predicate) {
        ch.select(entityClass, predicate);
        return typeThis;
    }

}
