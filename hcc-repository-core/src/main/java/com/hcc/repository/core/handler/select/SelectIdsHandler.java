package com.hcc.repository.core.handler.select;

import com.hcc.repository.core.conditions.ICondition;
import com.hcc.repository.core.conditions.query.DefaultQueryCondition;
import com.hcc.repository.core.handler.AbstractMethodHandler;
import com.hcc.repository.core.handler.AbstractSelectMethodHandler;
import com.hcc.repository.core.metadata.TableColumnInfo;
import com.hcc.repository.core.metadata.TableInfoHelper;
import com.hcc.repository.core.utils.CollUtils;
import com.hcc.repository.core.utils.ReflectUtils;

import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

/**
 * SelectIdsHandler
 *
 * @author hushengjun
 * @date 2023/3/21
 */
public class SelectIdsHandler extends AbstractSelectMethodHandler {

    @Override
    protected void prepare() {
    }

    @Override
    protected Object executeSql(String sql, Object[] args) {
        List<?> results = jdbcTemplateWrapper.queryForList(sql, args, entityClass);
        if (CollUtils.isEmpty(results)) {
            return Collections.emptyList();
        }

        TableColumnInfo idColumnInfo = TableInfoHelper.getIdColumnInfo(entityClass);

        return results.stream()
                .map(o -> ReflectUtils.getValue(o, idColumnInfo.getField()))
                .collect(Collectors.toList());
    }

}
