package com.hcc.repository.core.handler.update;

import com.hcc.repository.core.conditions.ICondition;
import com.hcc.repository.core.constants.ExecuteSqlTypeEnum;

/**
 * UpdateHandler
 *
 * @author hushengjun
 * @date 2023/3/21
 */
public class UpdateHandler extends AbstractUpdateHandler {

    @Override
    protected ICondition<?> prepareCondition() {
        ICondition<?> condition = getFirstArg(ICondition.class);
        condition.setExecuteSqlType(ExecuteSqlTypeEnum.UPDATE);

        return condition;
    }

}
