package com.hcc.repository.core.handler.delete;

import com.hcc.repository.core.conditions.ICondition;
import com.hcc.repository.core.conditions.update.DefaultUpdateCondition;
import com.hcc.repository.core.constants.ExecuteSqlTypeEnum;
import com.hcc.repository.core.handler.AbstractMethodHandler;
import com.hcc.repository.core.metadata.TableInfoHelper;

import java.util.Collection;

/**
 * DeleteByIdsHandler
 *
 * @author hushengjun
 * @date 2023/3/21
 */
public class DeleteByIdsHandler extends AbstractMethodHandler {

    @Override
    protected ICondition<?> prepareCondition() {
        Object firstArg = getFirstArg();
        String idColumnName = TableInfoHelper.getIdColumnName(entityClass);

        DefaultUpdateCondition<?> condition = new DefaultUpdateCondition<>(entityClass)
                .in(idColumnName, (Collection<?>) firstArg);
        condition.setExecuteSqlType(ExecuteSqlTypeEnum.DELETE);

        return condition;
    }

    @Override
    protected Object executeSql(String sql, Object[] args) {
        return jdbcOperations.update(sql, args);
    }

}
