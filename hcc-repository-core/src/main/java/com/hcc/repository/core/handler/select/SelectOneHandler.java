package com.hcc.repository.core.handler.select;

import com.hcc.repository.core.conditions.ICondition;
import com.hcc.repository.core.handler.AbstractMethodHandler;

/**
 * SelectOneHandler
 *
 * @author hushengjun
 * @date 2023/3/21
 */
public class SelectOneHandler extends AbstractMethodHandler {
    @Override
    protected Object handleMethod() throws Exception {
        ICondition<?> condition = getFirstArg(ICondition.class);

        return jdbcTemplateWrapper.namedQueryForObject(condition.getSqlQuery(), condition.getColumnValuePairs(), entityClass);
    }
}
