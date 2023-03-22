package com.hcc.repository.core.handler.select;

import com.hcc.repository.core.conditions.ICondition;
import com.hcc.repository.core.conditions.query.DefaultQueryCondition;
import com.hcc.repository.core.handler.AbstractMethodHandler;
import com.hcc.repository.core.metadata.TableColumnInfo;
import com.hcc.repository.core.metadata.TableInfoHelper;

/**
 * SelectByIdHandler
 *
 * @author hushengjun
 * @date 2023/3/21
 */
public class SelectByIdHandler extends AbstractMethodHandler {
    @Override
    protected Object handleMethod() throws Exception {
        Object firstArg = getFirstArg();
        TableColumnInfo idColumnInfo = TableInfoHelper.getIdColumnInfo(entityClass);
        if (idColumnInfo == null) {
            throw new RuntimeException("没有id");
        }
        ICondition<?> condition = new DefaultQueryCondition<>(entityClass)
                .eq(idColumnInfo.getColumnName(), firstArg);

        return jdbcTemplateWrapper.namedQueryForObject(condition.getSqlQuery(), condition.getColumnValuePairs(), entityClass);
    }
}
