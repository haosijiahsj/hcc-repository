package com.hcc.repository.core.handler.update;

import com.hcc.repository.core.conditions.ICondition;
import com.hcc.repository.core.handler.AbstractMethodHandler;

/**
 * UpdateHandler
 *
 * @author hushengjun
 * @date 2023/3/21
 */
public class UpdateHandler extends AbstractMethodHandler {

    @Override
    protected ICondition<?> assembleCondition() {
        return getFirstArg(ICondition.class);
    }

    @Override
    protected Object executeSql(String sql, Object[] args) {
        return jdbcTemplateProxy.update(sql, args);
    }

}
