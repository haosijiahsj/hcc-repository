package com.hcc.repository.core.handler.update;

import com.hcc.repository.core.conditions.ICondition;
import com.hcc.repository.core.conditions.update.AbstractUpdateCondition;
import com.hcc.repository.core.constants.ExecuteSqlTypeEnum;
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
        ICondition<?> condition = getFirstArg(ICondition.class);
        if (!(condition instanceof AbstractUpdateCondition)) {
            throw new UnsupportedOperationException("update仅支持使用Update的Condition");
        }
        condition.setExecuteSqlType(ExecuteSqlTypeEnum.UPDATE);

        return condition;
    }

    @Override
    protected Object executeSql(String sql, Object[] args) {
        return jdbcTemplateProxy.update(sql, args);
    }

}
