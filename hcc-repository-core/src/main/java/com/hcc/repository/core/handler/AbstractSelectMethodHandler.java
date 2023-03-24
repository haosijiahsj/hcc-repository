package com.hcc.repository.core.handler;

import com.hcc.repository.core.conditions.ICondition;
import com.hcc.repository.core.conditions.query.DefaultQueryCondition;

/**
 * AbstractCrudMethodHandler
 *
 * @author hushengjun
 * @date 2023/3/23
 */
public abstract class AbstractSelectMethodHandler extends AbstractMethodHandler {

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
