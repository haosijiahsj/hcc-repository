package com.hcc.repository.core.handler.insert;

import com.hcc.repository.core.conditions.ICondition;
import com.hcc.repository.core.conditions.insert.AbstractInsertCondition;
import com.hcc.repository.core.handler.AbstractMethodHandler;

/**
 * InsertByConditionHandler
 *
 * @author hushengjun
 * @date 2023/4/9
 */
public class InsertByConditionHandler extends AbstractMethodHandler {

    @Override
    protected ICondition<?> prepareCondition() {
        return getFirstArg(ICondition.class);
    }

    @Override
    protected Object executeSql(String sql, Object[] args) {
        return jdbcTemplateProxy.update(sql, args);
    }

}
