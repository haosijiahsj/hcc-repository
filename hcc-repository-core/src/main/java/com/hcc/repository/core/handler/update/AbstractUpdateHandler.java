package com.hcc.repository.core.handler.update;

import com.hcc.repository.core.conditions.ICondition;
import com.hcc.repository.core.handler.AbstractMethodHandler;

/**
 * AbstractUpdateHandler
 *
 * @author hushengjun
 * @date 2023/4/25
 */
public abstract class AbstractUpdateHandler extends AbstractMethodHandler {

    @Override
    protected abstract ICondition<?> prepareCondition();

    @Override
    protected Object executeSql(String sql, Object[] args) {
        return jdbcOperations.update(sql, args);
    }

}
