package com.hcc.repository.core.handler.select;

import com.hcc.repository.core.conditions.ICondition;
import com.hcc.repository.core.conditions.query.DefaultQueryCondition;
import com.hcc.repository.core.conditions.query.LambdaQueryCondition;
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
    @SuppressWarnings("unchecked")
    protected ICondition<?> prepareCondition() {
        ICondition<?> condition = super.prepareCondition();
        String idColumnName = TableInfoHelper.getIdColumnName(entityClass);
        if (condition instanceof DefaultQueryCondition) {
            DefaultQueryCondition<?> defaultQueryCondition = (DefaultQueryCondition<?>) condition;
            List<String> selectColumns = defaultQueryCondition.getSelectColumns();
            if (CollUtils.isEmpty(selectColumns) || !selectColumns.contains(idColumnName)) {
                defaultQueryCondition.select(idColumnName);
            }
        } else if (condition instanceof LambdaQueryCondition) {
            LambdaQueryCondition<?> lambdaQueryCondition = (LambdaQueryCondition<?>) condition;
            List<String> selectColumns = lambdaQueryCondition.getSelectColumns();
            if (CollUtils.isEmpty(selectColumns) || !selectColumns.contains(idColumnName)) {
                lambdaQueryCondition.select((Class) entityClass, t -> idColumnName.equals(t.getColumnName()));
            }
        }

        return condition;
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
