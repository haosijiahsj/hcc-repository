package com.hcc.repository.core.handler.insert;

import com.hcc.repository.core.conditions.insert.DefaultInsertCondition;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;

/**
 * BatchInsertHandler
 *
 * @author hushengjun
 * @date 2023/3/21
 */
public class BatchInsertHandler extends InsertHandler {
    @Override
    public Object handle() throws Exception {
        Object firstArg = args[0];
        if (firstArg == null) {
            throw new IllegalArgumentException("插入参数不能为空！");
        }

        Collection<?> entities = (Collection<?>) firstArg;
        List<Map<String, Object>> paramMaps = new ArrayList<>();
        String sql = "";
        for (Object entity : entities) {
            DefaultInsertCondition<?> condition = (DefaultInsertCondition<?>) super.assembleCondition(entity);
            paramMaps.add(condition.getColumnValuePairs());
            sql = condition.getSqlInsert();
        }

        return jdbcTemplateWrapper.namedBatchUpdate(sql, paramMaps);
    }
}
