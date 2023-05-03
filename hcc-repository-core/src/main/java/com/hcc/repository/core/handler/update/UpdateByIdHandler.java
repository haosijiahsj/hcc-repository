package com.hcc.repository.core.handler.update;

import com.hcc.repository.core.conditions.ICondition;
import com.hcc.repository.core.conditions.update.DefaultUpdateCondition;
import com.hcc.repository.core.metadata.TableColumnInfo;
import com.hcc.repository.core.metadata.TableInfoHelper;
import com.hcc.repository.core.utils.Assert;
import com.hcc.repository.core.utils.ReflectUtils;

import java.util.List;

/**
 * UpdateHandler
 *
 * @author hushengjun
 * @date 2023/3/21
 */
public class UpdateByIdHandler extends UpdateEntityHandler {

    @Override
    protected void prepare() {
        Assert.isFalse(firstArgIsNull(), "实体不能为空");
        Assert.isTrue(TableInfoHelper.hasIdColumn(entityClass),
                String.format("表：%s，没有定义id字段", TableInfoHelper.getTableName(entityClass)));
    }

    @Override
    protected ICondition<?> prepareCondition() {
        Object firstArg = getFirstArg();
        DefaultUpdateCondition<?> condition = new DefaultUpdateCondition<>(entityClass);
        // 使用对象拼接update sql, 主键、乐观锁字段不set条件
        List<TableColumnInfo> columnInfos = TableInfoHelper.getColumnInfos(entityClass, c -> !c.isPrimaryKey() && !c.isVersion());

        // set语句
        columnInfos.forEach(c -> condition.set(c.getColumnName(), super.processTargetValue(firstArg, c)));

        // 条件
        TableColumnInfo idColumnInfo = TableInfoHelper.getIdColumnInfo(entityClass);
        condition.eq(idColumnInfo.getColumnName(), ReflectUtils.getValue(firstArg, idColumnInfo.getField()));

        return condition;
    }

}
