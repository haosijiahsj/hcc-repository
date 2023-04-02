package com.hcc.repository.core.handler.select;

/**
 * SelectOneHandler
 *
 * @author hushengjun
 * @date 2023/3/21
 */
@Deprecated
public class SelectOneHandler extends AbstractSelectHandler {

    @Override
    protected void prepare() {}

    @Override
    protected Object executeSql(String sql, Object[] args) {
        return jdbcTemplateProxy.queryForEntityObj(sql, args, entityClass);
    }

}
