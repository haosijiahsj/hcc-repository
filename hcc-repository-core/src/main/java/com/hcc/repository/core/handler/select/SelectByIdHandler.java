package com.hcc.repository.core.handler.select;

import com.hcc.repository.core.conditions.ICondition;
import com.hcc.repository.core.conditions.query.DefaultQueryCondition;
import com.hcc.repository.core.handler.AbstractMethodHandler;
import com.hcc.repository.core.handler.AbstractSelectMethodHandler;
import com.hcc.repository.core.metadata.TableColumnInfo;
import com.hcc.repository.core.metadata.TableInfoHelper;

/**
 * SelectByIdHandler
 *
 * @author hushengjun
 * @date 2023/3/21
 */
public class SelectByIdHandler extends AbstractSelectMethodHandler {

    @Override
    protected ICondition<?> assembleCondition() {
        Object firstArg = getFirstArg();
        TableColumnInfo idColumnInfo = TableInfoHelper.getIdColumnInfo(entityClass);
        if (idColumnInfo == null) {
            throw new RuntimeException("没有id");
        }

        return new DefaultQueryCondition<>(entityClass)
                .eq(idColumnInfo.getColumnName(), firstArg);
    }

    @Override
    protected Object executeSql(String sql, Object[] args) {
        return jdbcTemplateWrapper.queryForObject(sql, args, entityClass);
    }

    @Override
    protected Object defaultValueForQuery() {
        return null;
    }

}
