package com.hcc.repository.core.handler.delete;

import com.hcc.repository.core.conditions.ICondition;
import com.hcc.repository.core.conditions.query.AbstractQueryCondition;
import com.hcc.repository.core.handler.select.AbstractSelectHandler;

/**
 * DeleteHandler
 *
 * @author hushengjun
 * @date 2023/3/21
 */
public class DeleteHandler extends AbstractSelectHandler {

    @Override
    protected ICondition<?> assembleCondition() {
        ICondition<?> condition = super.assembleCondition();
        if (!(condition instanceof AbstractQueryCondition)) {
            throw new UnsupportedOperationException("delete仅支持使用Query的Condition");
        }

        return condition;
    }

    @Override
    protected Object executeSql(String sql, Object[] args) {
        return jdbcTemplateProxy.update(sql, args);
    }

}
