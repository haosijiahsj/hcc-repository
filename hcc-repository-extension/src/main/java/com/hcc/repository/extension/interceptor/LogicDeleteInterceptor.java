package com.hcc.repository.extension.interceptor;

import com.hcc.repository.annotation.LogicDelValueType;
import com.hcc.repository.core.conditions.ICondition;
import com.hcc.repository.core.conditions.insert.AbstractInsertCondition;
import com.hcc.repository.core.conditions.query.AbstractQueryCondition;
import com.hcc.repository.core.conditions.update.AbstractUpdateCondition;
import com.hcc.repository.core.constants.ExecuteSqlTypeEnum;
import com.hcc.repository.core.constants.MethodNameEnum;
import com.hcc.repository.core.constants.SqlTypeEnum;
import com.hcc.repository.core.interceptor.Interceptor;
import com.hcc.repository.core.metadata.TableColumnInfo;
import com.hcc.repository.core.metadata.TableInfo;
import com.hcc.repository.core.metadata.TableInfoHelper;
import com.hcc.repository.core.utils.Assert;
import lombok.extern.slf4j.Slf4j;

import java.util.Optional;
import java.util.UUID;
import java.util.function.Predicate;

/**
 * 逻辑删除拦截器<br/>
 * 不支持自行定义的sql
 *
 * @author hushengjun
 * @date 2023/4/8
 */
@Slf4j
public class LogicDeleteInterceptor implements ExtInterceptor {

    private final Predicate<TableInfo> predicate;

    public LogicDeleteInterceptor() {
        predicate = t -> true;
    }

    public LogicDeleteInterceptor(Predicate<TableInfo> predicate) {
        this.predicate = Optional.ofNullable(predicate).orElse(t -> true);
    }

    @Override
    public void afterPrepareCondition(MethodNameEnum methodNameEnum, Object[] args, ICondition<?> condition) {
        Class<?> entityClass = condition.getEntityClass();
        if (entityClass == null) {
            return;
        }
        TableInfo tableInfo = TableInfoHelper.getTableInfo(entityClass);
        if (!predicate.test(tableInfo)) {
            log.debug("table name: {}忽略逻辑删除拦截器", tableInfo.getTableName());
            return;
        }

        boolean hasLogicDeleteColumn = TableInfoHelper.hasLogicDeleteColumn(entityClass);
        if (!hasLogicDeleteColumn) {
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
        SqlTypeEnum sqlType = methodNameEnum.getSqlType();
        if (SqlTypeEnum.INSERT.equals(sqlType)) {
            if (condition instanceof AbstractInsertCondition) {
                // 插入语句覆盖掉逻辑未删除值
                AbstractInsertCondition<?, ?, ?> insertCondition = (AbstractInsertCondition<?, ?, ?>) condition;
                if (!insertCondition.getSqlColumns().contains(columnName)) {
                    insertCondition.getSqlColumns().add(columnName);
                }
                insertCondition.getColumnValuePairs().put(columnName, logicNotDelVal);
            }
        } else if (SqlTypeEnum.DELETE.equals(sqlType)) {
            // 改写为更新语句
            condition.setExecuteSqlType(ExecuteSqlTypeEnum.UPDATE);
            if (condition instanceof AbstractUpdateCondition) {
                ((AbstractUpdateCondition<?, ?, ?>) condition).setSql(String.format("%s = " + formatField, columnName, logicDelVal));
                ((AbstractUpdateCondition<?, ?, ?>) condition).apply(whereSqlSegment);
            }
        } else if (SqlTypeEnum.UPDATE.equals(sqlType)) {
            if (condition instanceof AbstractUpdateCondition) {
                ((AbstractUpdateCondition<?, ?, ?>) condition).apply(whereSqlSegment);
            }
        } else {
            if (condition instanceof AbstractQueryCondition) {
                ((AbstractQueryCondition<?, ?, ?>) condition).apply(whereSqlSegment);
            }
        }
    }

}
