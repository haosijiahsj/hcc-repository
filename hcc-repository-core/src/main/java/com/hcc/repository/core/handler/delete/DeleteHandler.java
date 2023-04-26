package com.hcc.repository.core.handler.delete;

import com.hcc.repository.core.handler.select.AbstractSelectHandler;

/**
 * DeleteHandler
 *
 * @author hushengjun
 * @date 2023/3/21
 */
public class DeleteHandler extends AbstractSelectHandler {

    @Override
    protected Object executeSql(String sql, Object[] args) {
        return jdbcOperations.update(sql, args);
    }

}
