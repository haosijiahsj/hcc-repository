package com.hcc.repository.core.handler.update;

import com.hcc.repository.annotation.IEnum;
import com.hcc.repository.core.conditions.ICondition;
import com.hcc.repository.core.conditions.update.DefaultUpdateCondition;
import com.hcc.repository.core.handler.AbstractMethodHandler;
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
public class UpdateByIdHandler extends AbstractMethodHandler {

    @Override
    protected void prepare() {
        Assert.isTrue(TableInfoHelper.hasIdColumn(entityClass),
                String.format("表：%s，没有定义id字段", TableInfoHelper.getTableName(entityClass)));
    }

    @Override
    @SuppressWarnings("unchecked")
    protected ICondition<?> prepareCondition() {
        Object firstArg = getFirstArg();
        DefaultUpdateCondition<?> condition = new DefaultUpdateCondition<>(entityClass);
        // 使用对象拼接update sql
        List<TableColumnInfo> columnInfos = TableInfoHelper.getColumnInfosWithOutIdColumn(entityClass);
        // set语句
        columnInfos.forEach(c -> {
            Object value = ReflectUtils.getValue(firstArg, c.getField());
            // 转换
            Object targetValue = value;
            if (c.needConvert()) {
                targetValue = ReflectUtils.newInstance(c.getConverter()).convertToColumn(value);
            } else if (c.isAssignableFromIEnum()) {
                targetValue = ((IEnum)value).getValue();
            }
            condition.set(c.getColumnName(), targetValue);
        });
        TableColumnInfo idColumnInfo = TableInfoHelper.getIdColumnInfo(entityClass);
        condition.eq(idColumnInfo.getColumnName(), ReflectUtils.getValue(firstArg, idColumnInfo.getField()));

        return condition;
    }

    @Override
    protected Object executeSql(String sql, Object[] args) {
        return jdbcTemplateProxy.update(sql, args);
    }

}
