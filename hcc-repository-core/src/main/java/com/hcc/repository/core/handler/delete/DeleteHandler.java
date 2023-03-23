package com.hcc.repository.core.handler.delete;

import com.hcc.repository.core.handler.AbstractSelectMethodHandler;

/**
 * DeleteHandler
 *
 * @author hushengjun
 * @date 2023/3/21
 */
public class DeleteHandler extends AbstractSelectMethodHandler {

    @Override
    protected Object executeSql(String sql, Object[] args) {
        return jdbcTemplateWrapper.update(sql, args);
    }
}
