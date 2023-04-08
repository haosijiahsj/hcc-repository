package com.hcc.repository.extension.interceptor;

import com.hcc.repository.annotation.LogicDelValueType;
import com.hcc.repository.core.conditions.AbstractCondition;
import com.hcc.repository.core.conditions.ICondition;
import com.hcc.repository.core.conditions.query.AbstractQueryCondition;
import com.hcc.repository.core.conditions.update.AbstractUpdateCondition;
import com.hcc.repository.core.constants.ExecuteSqlTypeEnum;
import com.hcc.repository.core.constants.MethodNameEnum;
import com.hcc.repository.core.constants.SqlTypeEnum;
import com.hcc.repository.core.interceptor.Interceptor;
import com.hcc.repository.core.metadata.TableColumnInfo;
import com.hcc.repository.core.metadata.TableInfoHelper;
import com.hcc.repository.core.utils.Assert;

import java.util.UUID;

/**
 * LogicDeleteInterceptor
 *
 * @author hushengjun
 * @date 2023/4/8
 */
public class LogicDeleteInterceptor implements Interceptor {

    @Override
    public void afterPrepareCondition(MethodNameEnum methodNameEnum, ICondition<?> condition) {
        Class<?> entityClass = condition.getEntityClass();
        boolean hasLogicDeleteColumn = TableInfoHelper.hasLogicDeleteColumn(entityClass);
        if (!hasLogicDeleteColumn) {
            return;
        }

        SqlTypeEnum sqlType = methodNameEnum.getSqlType();
        if (SqlTypeEnum.INSERT.equals(sqlType)) {
            return;
        }
        TableColumnInfo logicDelColumnInfo = TableInfoHelper.getLogicDeleteColumnInfo(entityClass);
        Assert.isNotNull(logicDelColumnInfo, "未获取到logic delete列信息");

        String columnName = logicDelColumnInfo.getColumnName();
        String logicDelVal = logicDelColumnInfo.getLogicDelVal();
        String logicNotDelVal = logicDelColumnInfo.getLogicNotDelVal();

        // 这里是处理逻辑删除字段作为唯一索引情况
        if (LogicDelValueType.TIMESTAMP.equals(logicDelColumnInfo.getLogicDelValueType())) {
            logicDelVal = String.valueOf(System.nanoTime());
        } else if (LogicDelValueType.UUID.equals(logicDelColumnInfo.getLogicDelValueType())) {
            logicDelVal = UUID.randomUUID().toString().replaceAll("-", "");
        }

        String formatField = "%s";
        if (String.class.equals(logicDelColumnInfo.getField().getType())) {
            // 字符串类型加上单引号
            formatField = "'%s'";
        }
        String whereSqlSegment = String.format("AND %s = " + formatField, columnName, logicNotDelVal);
        // 通过condition判断并加入逻辑删除条件
        if (SqlTypeEnum.DELETE.equals(sqlType)) {
            // 改写为更新语句
            condition.setExecuteSqlType(ExecuteSqlTypeEnum.UPDATE);
            if (condition instanceof AbstractUpdateCondition) {
                ((AbstractUpdateCondition) condition).setSql(String.format("%s = " + formatField, columnName, logicDelVal));
                ((AbstractUpdateCondition) condition).apply(whereSqlSegment);
            } else {
                throw new IllegalArgumentException("逻辑删除时，仅支持使用Update的Condition");
            }
        } else if (SqlTypeEnum.UPDATE.equals(sqlType)) {
            ((AbstractUpdateCondition) condition).apply(whereSqlSegment);
        } else {
            ((AbstractQueryCondition) condition).apply(whereSqlSegment);
        }
    }

}
