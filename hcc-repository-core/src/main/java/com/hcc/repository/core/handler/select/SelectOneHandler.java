package com.hcc.repository.core.handler.select;

import com.hcc.repository.core.handler.AbstractSelectMethodHandler;

/**
 * SelectOneHandler
 *
 * @author hushengjun
 * @date 2023/3/21
 */
public class SelectOneHandler extends AbstractSelectMethodHandler {

    @Override
    protected void prepare() {
    }

    @Override
    protected Object executeSql(String sql, Object[] args) {
        return jdbcTemplateWrapper.queryForObject(sql, args, entityClass);
    }

    @Override
    protected Object defaultValueForQuery() {
        return null;
    }

}
