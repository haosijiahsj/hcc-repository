package com.hcc.repository.core.handler.select;

/**
 * 分页查询handler
 *
 * @author hushengjun
 * @date 2023/3/26
 */
public class SelectMapsPageHandler extends AbstractSelectHandler {

    @Override
    protected void prepare() {}

    @Override
    protected Object executeSql(String sql, Object[] args) {
        return jdbcOperations.queryForList(sql, args);
    }

}
