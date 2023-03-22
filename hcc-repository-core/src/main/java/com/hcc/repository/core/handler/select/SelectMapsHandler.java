package com.hcc.repository.core.handler.select;

import com.hcc.repository.core.conditions.ICondition;
import com.hcc.repository.core.conditions.query.DefaultQueryCondition;
import com.hcc.repository.core.handler.AbstractMethodHandler;

/**
 * SelectOneHandler
 *
 * @author hushengjun
 * @date 2023/3/21
 */
public class SelectMapsHandler extends AbstractMethodHandler {
    @Override
    protected Object handleMethod() throws Exception {
        ICondition<?> condition;
        if (firstArgIsNull()) {
            condition = new DefaultQueryCondition<>(entityClass);
        } else {
            condition = getFirstArg(ICondition.class);
            condition.setEntityClass(entityClass);
        }

        return jdbcTemplateWrapper.namedQueryForList(condition.getSqlQuery(), condition.getColumnValuePairs());
    }
}
