package com.hcc.repository.core.handler.select;

import com.hcc.repository.core.conditions.ICondition;
import com.hcc.repository.core.conditions.query.DefaultQueryCondition;
import com.hcc.repository.core.metadata.TableColumnInfo;
import com.hcc.repository.core.metadata.TableInfoHelper;
import com.hcc.repository.core.utils.Assert;

import java.util.Collection;

/**
 * SelectByIdHandler
 *
 * @author hushengjun
 * @date 2023/3/21
 */
public class SelectByIdsHandler extends AbstractSelectHandler {

    @Override
    protected void prepare() {
        Assert.isTrue(TableInfoHelper.hasIdColumn(entityClass),
                String.format("表：%s，没有定义id字段", TableInfoHelper.getTableName(entityClass)));
    }

    @Override
    protected ICondition<?> prepareCondition() {
        Object firstArg = getFirstArg();
        TableColumnInfo idColumnInfo = TableInfoHelper.getIdColumnInfo(entityClass);

        return new DefaultQueryCondition<>(entityClass)
                // 如果不强转，会调用可变参数的in方法，导致解析sql后in中只有一个参数
                .in(idColumnInfo.getColumnName(), (Collection<?>) firstArg);
    }

    @Override
    protected Object executeSql(String sql, Object[] args) {
        return jdbcOperations.queryForEntityList(sql, args, entityClass);
    }

}
