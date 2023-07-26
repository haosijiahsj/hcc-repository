package com.hcc.repository.core.handler.update;

import com.hcc.repository.annotation.AutoFillContext;
import com.hcc.repository.annotation.AutoFillStrategy;
import com.hcc.repository.annotation.IConverter;
import com.hcc.repository.core.conditions.AbstractCondition;
import com.hcc.repository.core.conditions.ICondition;
import com.hcc.repository.core.conditions.SegmentContainer;
import com.hcc.repository.core.conditions.update.DefaultUpdateCondition;
import com.hcc.repository.core.convert.IEnumConverter;
import com.hcc.repository.core.metadata.TableColumnInfo;
import com.hcc.repository.core.metadata.TableInfo;
import com.hcc.repository.core.metadata.TableInfoHelper;
import com.hcc.repository.core.utils.Assert;
import com.hcc.repository.core.utils.ConstructorUtils;
import com.hcc.repository.core.utils.ReflectUtils;

import java.util.List;
import java.util.Optional;

/**
 * UpdateEntityHandler
 *
 * @author hushengjun
 * @date 2023/4/25
 */
public class UpdateEntityHandler extends AbstractUpdateHandler {

    @Override
    protected void prepare() {
        Assert.isFalse(firstArgIsNull(), "实体不能为空！");
    }

    @Override
    protected ICondition<?> prepareCondition() {
        Object entity = getFirstArg();
        ICondition<?> originalCondition = getArg(1, ICondition.class);
        boolean nullSet = getArg(2, Boolean.class);

        DefaultUpdateCondition<?> condition = new DefaultUpdateCondition<>(entityClass);
        // 主键、乐观锁字段不set条件
        List<TableColumnInfo> columnInfos = TableInfoHelper.getColumnInfos(entityClass, c -> !c.isPrimaryKey() && !c.isVersion());
        // set语句
        for (TableColumnInfo c : columnInfos) {
            Object targetValue = getColumnValue(entity, c);
            if (targetValue == null && !nullSet) {
                continue;
            }
            condition.set(c.getColumnName(), targetValue);
        }
        if (originalCondition instanceof AbstractCondition) {
            // 直接赋值segmentContainer
            AbstractCondition<?, ?, ?> abstractCondition = (AbstractCondition<?, ?, ?>) originalCondition;
            SegmentContainer segmentContainer = (abstractCondition).getSegmentContainer();
            condition.setSegmentContainer(segmentContainer);
            condition.getColumnValuePairs().putAll(abstractCondition.getColumnValuePairs());
        }

        return condition;
    }

    /**
     * 获取列值
     * @param entity
     * @param columnInfo
     * @return
     */
    @SuppressWarnings("unchecked")
    protected Object getColumnValue(Object entity, TableColumnInfo columnInfo) {
        Object value = ReflectUtils.getValue(entity, columnInfo.getField());
        // 转换
        Object targetValue = value;
        if (value != null) {
            Class<? extends IConverter> converter = null;
            if (columnInfo.needConvert()) {
                converter = columnInfo.getConverter();
            } else if (columnInfo.isAssignableFromIEnum()) {
                converter = IEnumConverter.class;
            }
            if (converter != null) {
                targetValue = this.newInstanceConverter(converter, columnInfo.getField().getType()).convertToColumn(value);
            }
        }
        if (targetValue == null && columnInfo.needAutoFillUpdate()) {
            targetValue = this.getUpdateAutoFillValue(TableInfoHelper.getTableInfo(entityClass), columnInfo);
        }

        return targetValue;
    }

    /**
     * 实例化converter
     * @param converterClass
     * @param targetClass
     * @return
     */
    private IConverter newInstanceConverter(Class<? extends IConverter> converterClass, Class<?> targetClass) {
        return Optional.ofNullable(ReflectUtils.matchConstruct(converterClass, Class.class))
                .map(c -> (IConverter) ConstructorUtils.newInstance(c, targetClass))
                .orElseGet(() -> ReflectUtils.newInstance(converterClass));
    }

    /**
     * 获取填充值
     * @param tableInfo
     * @param columnInfo
     * @return
     */
    private Object getUpdateAutoFillValue(TableInfo tableInfo, TableColumnInfo columnInfo) {
        AutoFillContext context = new AutoFillContext();
        context.setFieldName(columnInfo.getFieldName());
        context.setColumnName(columnInfo.getColumnName());
        context.setFieldType(columnInfo.getField().getType());
        context.setTableName(tableInfo.getTableName());
        context.setEntityClass(tableInfo.getClazz());

        AutoFillStrategy autoFillStrategy = ReflectUtils.newInstanceForCache(columnInfo.getUpdateStrategy());
        if (!autoFillStrategy.autoFill(context)) {
            return null;
        }

        return autoFillStrategy.fill(context);
    }

}
