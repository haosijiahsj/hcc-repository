package com.hcc.repository.core.handler.select;

import com.hcc.repository.core.conditions.ICondition;
import com.hcc.repository.core.conditions.query.DefaultQueryCondition;
import com.hcc.repository.core.handler.AbstractMethodHandler;

/**
 * AbstractCrudMethodHandler
 *
 * @author hushengjun
 * @date 2023/3/23
 */
public abstract class AbstractSelectHandler extends AbstractMethodHandler {

    @Override
    protected ICondition<?> assembleCondition() {
        ICondition<?> condition;
        if (firstArgIsNull()) {
            condition = new DefaultQueryCondition<>(entityClass);
        } else {
            condition = getFirstArg(ICondition.class);
        }

        return condition;
    }

}
