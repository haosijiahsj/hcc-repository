package com.hcc.repository.core.handler.select;

import com.hcc.repository.core.conditions.ICondition;
import com.hcc.repository.core.conditions.query.DefaultQueryCondition;
import com.hcc.repository.core.handler.AbstractMethodHandler;
import com.hcc.repository.core.handler.AbstractSelectMethodHandler;

/**
 * SelectOneHandler
 *
 * @author hushengjun
 * @date 2023/3/21
 */
public class SelectListHandler extends AbstractSelectMethodHandler {
    @Override
    protected void prepare() {
    }

    @Override
    protected Object executeSql(String sql, Object[] args) {
        return jdbcTemplateWrapper.queryForList(sql, args, entityClass);
    }

}
