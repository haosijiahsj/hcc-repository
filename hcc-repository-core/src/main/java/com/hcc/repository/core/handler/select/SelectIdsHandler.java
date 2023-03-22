package com.hcc.repository.core.handler.select;

import com.hcc.repository.core.conditions.ICondition;
import com.hcc.repository.core.conditions.query.DefaultQueryCondition;
import com.hcc.repository.core.handler.AbstractMethodHandler;
import com.hcc.repository.core.metadata.TableColumnInfo;
import com.hcc.repository.core.metadata.TableInfoHelper;
import com.hcc.repository.core.utils.CollUtils;
import com.hcc.repository.core.utils.ReflectUtils;

import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

/**
 * SelectOneHandler
 *
 * @author hushengjun
 * @date 2023/3/21
 */
public class SelectIdsHandler extends AbstractMethodHandler {
    @Override
    protected Object handleMethod() throws Exception {
        if (!TableInfoHelper.hasIdColumn(entityClass)) {
            throw new RuntimeException("没有id列");
        }
        ICondition<?> condition;
        if (firstArgIsNull()) {
            condition = new DefaultQueryCondition<>(entityClass);
        } else {
            condition = getFirstArg(ICondition.class);
            condition.setEntityClass(entityClass);
        }

        List<?> results = jdbcTemplateWrapper.namedQueryForList(condition.getSqlQuery(), condition.getColumnValuePairs(), entityClass);
        if (CollUtils.isEmpty(results)) {
            return Collections.emptyList();
        }

        TableColumnInfo idColumnInfo = TableInfoHelper.getIdColumnInfo(entityClass);

        return results.stream()
                .map(o -> ReflectUtils.getValue(o, idColumnInfo.getField()))
                .collect(Collectors.toList());
    }
}
