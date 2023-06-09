package com.hcc.repository.core.handler.select;

/**
 * 分页查询handler
 *
 * @author hushengjun
 * @date 2023/3/26
 */
public class SelectPageHandler extends AbstractSelectHandler {

    @Override
    protected void prepare() {}

    @Override
    protected Object executeSql(String sql, Object[] args) {
        return jdbcOperations.queryForEntityList(sql, args, entityClass);
    }

}
