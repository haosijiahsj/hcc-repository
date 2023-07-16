package com.hcc.repository.core.handler.select;

/**
 * SelectOneHandler
 *
 * @author hushengjun
 * @date 2023/3/21
 */
public class SelectCountHandler extends AbstractSelectHandler {

    @Override
    protected void prepare() {}

    @Override
    protected Object executeSql(String sql, Object[] args) {
        return jdbcOperations.queryForObject(sql, args, Long.class);
    }

}
