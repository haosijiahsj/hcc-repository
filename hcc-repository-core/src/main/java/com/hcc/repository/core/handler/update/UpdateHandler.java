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
    protected Object handleMethod() throws Exception {
        ICondition<?> condition = getFirstArg(ICondition.class);
        return jdbcTemplateWrapper.namedUpdate(condition.getSqlUpdate(), condition.getColumnValuePairs());
    }

}
