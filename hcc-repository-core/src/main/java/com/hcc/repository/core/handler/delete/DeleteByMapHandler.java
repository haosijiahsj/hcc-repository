package com.hcc.repository.core.handler.delete;

import com.hcc.repository.core.conditions.ICondition;
import com.hcc.repository.core.conditions.update.DefaultUpdateCondition;
import com.hcc.repository.core.constants.ExecuteSqlTypeEnum;
import com.hcc.repository.core.handler.AbstractMethodHandler;
import com.hcc.repository.core.utils.CollUtils;

import java.util.Collection;
import java.util.Map;

/**
 * DeleteByMapHandler
 *
 * @author hushengjun
 * @date 2023/7/11
 */
public class DeleteByMapHandler extends AbstractMethodHandler {

    @Override
    protected void prepare() {}

    @Override
    @SuppressWarnings("unchecked")
    protected ICondition<?> prepareCondition() {
        DefaultUpdateCondition<?> condition = new DefaultUpdateCondition<>(entityClass);
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
        condition.setExecuteSqlType(ExecuteSqlTypeEnum.DELETE);

        return condition;
    }

    @Override
    protected Object executeSql(String sql, Object[] args) {
        return jdbcOperations.update(sql, args);
    }

}
