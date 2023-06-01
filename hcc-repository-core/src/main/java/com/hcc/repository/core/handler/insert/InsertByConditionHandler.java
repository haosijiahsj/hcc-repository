package com.hcc.repository.core.handler.insert;

import com.hcc.repository.core.conditions.ICondition;
import com.hcc.repository.core.conditions.insert.AbstractInsertCondition;
import com.hcc.repository.core.handler.AbstractMethodHandler;
import com.hcc.repository.core.utils.Assert;

/**
 * InsertByConditionHandler
 *
 * @author hushengjun
 * @date 2023/4/9
 */
public class InsertByConditionHandler extends AbstractMethodHandler {

    @Override
    protected void prepare() {
        ICondition<?> condition = getFirstArg(ICondition.class);
        Assert.isFalse(firstArgIsNull(), "condition不能为空！");
        Assert.isTrue(condition instanceof AbstractInsertCondition, "condition必须是Insert的condition");
    }

    @Override
    protected ICondition<?> prepareCondition() {
        return getFirstArg(ICondition.class);
    }

    @Override
    protected Object executeSql(String sql, Object[] args) {
        return jdbcOperations.update(sql, args);
    }

}
