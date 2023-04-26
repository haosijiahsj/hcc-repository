package com.hcc.repository.core.handler.select;

import com.hcc.repository.core.conditions.ICondition;
import com.hcc.repository.core.conditions.query.DefaultQueryCondition;
import com.hcc.repository.core.handler.AbstractMethodHandler;
import com.hcc.repository.core.utils.CollUtils;

import java.util.Collection;
import java.util.Map;

/**
 * SelectListByMapHandler
 *
 * @author hushengjun
 * @date 2023/3/25
 */
public class SelectListByMapHandler extends AbstractMethodHandler {

    @Override
    @SuppressWarnings("unchecked")
    protected ICondition<?> prepareCondition() {
        DefaultQueryCondition<?> condition = new DefaultQueryCondition<>();
        if (!firstArgIsNull()) {
            Map<String, Object> paramMap = (Map<String, Object>) getFirstArg();
            if (CollUtils.isNotEmpty(paramMap)) {
                paramMap.forEach((k, v) -> {
                    if (v instanceof Collection) {
                        condition.in(k, (Collection<?>) v);
                    } else if (v.getClass().isArray()) {
                        condition.in(k, (Object[]) v);
                    } else {
                        condition.eq(k, v);
                    }
                });
            }
        }

        return condition;
    }

    @Override
    protected Object executeSql(String sql, Object[] args) {
        return jdbcOperations.queryForEntityList(sql, args, entityClass);
    }

}
