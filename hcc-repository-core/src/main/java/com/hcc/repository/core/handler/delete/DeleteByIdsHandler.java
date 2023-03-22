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
    protected Object handleMethod() throws Exception {
        Object firstArg = getFirstArg();
        String idColumnName = TableInfoHelper.getIdColumnName(entityClass);
        ICondition<?> condition = new DefaultQueryCondition<>(entityClass)
                .in(idColumnName, firstArg);

        return jdbcTemplateWrapper.namedUpdate(condition.getSqlDelete(), condition.getColumnValuePairs());
    }
}
