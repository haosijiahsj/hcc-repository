package com.hcc.repository.core.handler.delete;

import com.hcc.repository.core.conditions.ICondition;
import com.hcc.repository.core.handler.AbstractMethodHandler;
import com.hcc.repository.core.metadata.TableInfoHelper;

/**
 * DeleteHandler
 *
 * @author hushengjun
 * @date 2023/3/21
 */
public class DeleteHandler extends AbstractMethodHandler {
    @Override
    protected Object handleMethod() throws Exception {
        ICondition<?> condition = getFirstArg(ICondition.class);

        String namedSql = "DELETE FROM " + TableInfoHelper.getTableName(entityClass) + " " + condition.getSqlWhere();

        return jdbcTemplateWrapper.namedUpdate(namedSql, condition.getColumnValuePairs());
    }
}
