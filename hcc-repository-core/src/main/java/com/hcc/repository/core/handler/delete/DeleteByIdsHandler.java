package com.hcc.repository.core.handler.delete;

import com.hcc.repository.core.conditions.ICondition;
import com.hcc.repository.core.conditions.query.DefaultQueryCondition;
import com.hcc.repository.core.handler.AbstractMethodHandler;
import com.hcc.repository.core.metadata.TableInfoHelper;

/**
 * DeleteByIdsHandler
 *
 * @author hushengjun
 * @date 2023/3/21
 */
public class DeleteByIdsHandler extends AbstractMethodHandler {
    @Override
    protected ICondition<?> assembleCondition() {
        Object firstArg = getFirstArg();
        String idColumnName = TableInfoHelper.getIdColumnName(entityClass);

        return new DefaultQueryCondition<>(entityClass)
                .in(idColumnName, firstArg);
    }

    @Override
    protected Object executeSql(String sql, Object[] args) {
        return jdbcTemplateWrapper.update(sql, args);
    }

}
