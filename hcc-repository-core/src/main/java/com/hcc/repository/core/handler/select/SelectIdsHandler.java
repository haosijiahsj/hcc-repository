package com.hcc.repository.core.handler.select;

import com.hcc.repository.core.conditions.AbstractCondition;
import com.hcc.repository.core.conditions.ICondition;
import com.hcc.repository.core.conditions.query.DefaultQueryCondition;
import com.hcc.repository.core.metadata.TableColumnInfo;
import com.hcc.repository.core.metadata.TableInfoHelper;
import com.hcc.repository.core.utils.Assert;
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
public class SelectIdsHandler extends AbstractSelectHandler {

    @Override
    protected void prepare() {
        Assert.isTrue(TableInfoHelper.hasIdColumn(entityClass),
                String.format("表：%s，没有定义id字段", TableInfoHelper.getTableName(entityClass)));
    }

    @Override
    protected ICondition<?> prepareCondition() {
        ICondition<?> condition = super.prepareCondition();
        String idColumnName = TableInfoHelper.getIdColumnName(entityClass);

        // 利用原始的Condition重新构建一个Condition
        AbstractCondition<?, ?, ?> abstractCondition = (AbstractCondition<?, ?, ?>) condition;
        DefaultQueryCondition<?> newCondition = new DefaultQueryCondition<>();
        newCondition.select(idColumnName);
        newCondition.setSegmentContainer(abstractCondition.getSegmentContainer());
        newCondition.getColumnValuePairs().putAll(abstractCondition.getColumnValuePairs());

        return newCondition;
    }

    @Override
    protected Object executeSql(String sql, Object[] args) {
        List<?> results = jdbcOperations.queryForEntityList(sql, args, entityClass);
        if (CollUtils.isEmpty(results)) {
            return Collections.emptyList();
        }

        TableColumnInfo idColumnInfo = TableInfoHelper.getIdColumnInfo(entityClass);

        return results.stream()
                .map(o -> ReflectUtils.getValue(o, idColumnInfo.getField()))
                .collect(Collectors.toList());
    }

}
