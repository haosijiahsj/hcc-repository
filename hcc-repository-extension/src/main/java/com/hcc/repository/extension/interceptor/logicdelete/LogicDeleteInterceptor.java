package com.hcc.repository.extension.interceptor.logicdelete;

import com.hcc.repository.annotation.LogicDelValueType;
import com.hcc.repository.core.conditions.ICondition;
import com.hcc.repository.core.conditions.insert.AbstractInsertCondition;
import com.hcc.repository.core.conditions.query.AbstractQueryCondition;
import com.hcc.repository.core.conditions.update.AbstractUpdateCondition;
import com.hcc.repository.core.constants.ExecuteSqlTypeEnum;
import com.hcc.repository.core.constants.MethodNameEnum;
import com.hcc.repository.core.constants.SqlTypeEnum;
import com.hcc.repository.core.constants.StrPool;
import com.hcc.repository.core.metadata.TableColumnInfo;
import com.hcc.repository.core.metadata.TableInfo;
import com.hcc.repository.core.metadata.TableInfoHelper;
import com.hcc.repository.core.utils.Assert;
import com.hcc.repository.extension.interceptor.ExtInterceptor;
import lombok.extern.slf4j.Slf4j;

import java.lang.reflect.Method;
import java.util.HashSet;
import java.util.Optional;
import java.util.Set;
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

    private static final String logicDeleteColumnName = "HCC_LOGIC_DELETE_COLUMN";
    private static final Set<String> trueValues = new HashSet<>(4);
    private static final Set<String> falseValues = new HashSet<>(4);

    static {
        trueValues.add("true");
        trueValues.add("on");
        trueValues.add("yes");
        trueValues.add("1");

        falseValues.add("false");
        falseValues.add("off");
        falseValues.add("no");
        falseValues.add("0");
    }

    private final Predicate<TableInfo> predicate;

    public LogicDeleteInterceptor() {
        predicate = t -> true;
    }

    public LogicDeleteInterceptor(Predicate<TableInfo> predicate) {
        this.predicate = Optional.ofNullable(predicate).orElse(t -> true);
    }

    @Override
    public void afterPrepareCondition(Method method, Object[] parameters, ICondition<?> condition) {
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
        Object logicDelVal = this.getLogicDelVal(logicDelColumnInfo);
        Object logicNotDelVal = this.getLogicNotDelVal(logicDelColumnInfo);

//        String notDelColumnName = logicDeleteColumnName + "_NOT_DEL";
//        String delColumnName = logicDeleteColumnName + "_DEL";
        String whereSqlSegment = String.format("AND %s = %s", columnName,
                logicNotDelVal instanceof String ? String.format("'%s'", logicNotDelVal) : logicNotDelVal);
        // 通过condition判断并加入逻辑删除条件
        MethodNameEnum methodNameEnum = MethodNameEnum.get(method.getName());
        if (methodNameEnum == null) {
            return;
        }
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
            condition.setExecuteSqlType(ExecuteSqlTypeEnum.UPDATE);
            if (condition instanceof AbstractUpdateCondition) {
                AbstractUpdateCondition<?, ?, ?> abstractUpdateCondition = (AbstractUpdateCondition<?, ?, ?>) condition;
                if (logicDelVal instanceof String) {
                    logicDelVal = String.format("'%s'", logicDelVal);
                }
                abstractUpdateCondition.setSql(String.format("%s = %s", columnName, logicDelVal));
//                abstractUpdateCondition.setSql(String.format("%s = %s", columnName, StrPool.getPlaceholder(delColumnName)));
//                abstractUpdateCondition.getColumnValuePairs().put(delColumnName, logicDelVal);

                abstractUpdateCondition.apply(whereSqlSegment);
//                abstractUpdateCondition.getColumnValuePairs().put(notDelColumnName, logicNotDelVal);
            }
        } else if (SqlTypeEnum.UPDATE.equals(sqlType)) {
            if (condition instanceof AbstractUpdateCondition) {
                AbstractUpdateCondition<?, ?, ?> abstractUpdateCondition = (AbstractUpdateCondition<?, ?, ?>) condition;
                abstractUpdateCondition.apply(whereSqlSegment);
//                abstractUpdateCondition.getColumnValuePairs().put(notDelColumnName, logicNotDelVal);
            }
        } else {
            if (condition instanceof AbstractQueryCondition) {
                AbstractQueryCondition<?, ?, ?> abstractQueryCondition = (AbstractQueryCondition<?, ?, ?>) condition;
                abstractQueryCondition.apply(whereSqlSegment);
//                abstractQueryCondition.getColumnValuePairs().put(notDelColumnName, logicNotDelVal);
            }
        }
    }

    /**
     * 获取逻辑删除值
     * @param logicDelColumnInfo
     * @return
     */
    private Object getLogicDelVal(TableColumnInfo logicDelColumnInfo) {
        String logicDelVal = logicDelColumnInfo.getLogicDelVal();
        if (LogicDelValueType.SPECIFY.equals(logicDelColumnInfo.getLogicDelValueType())) {
            Class<?> fieldType = logicDelColumnInfo.getField().getType();

            return this.convertValue(logicDelVal, fieldType);
        }

        // 这里是处理逻辑删除字段作为唯一索引情况
        if (LogicDelValueType.TIMESTAMP.equals(logicDelColumnInfo.getLogicDelValueType())) {
            logicDelVal = String.valueOf(System.nanoTime());
        } else if (LogicDelValueType.UUID.equals(logicDelColumnInfo.getLogicDelValueType())) {
            logicDelVal = UUID.randomUUID().toString().replace("-", "");
        }

        return logicDelVal;
    }

    /**
     * 获取逻辑未删除值
     * @param logicDelColumnInfo
     * @return
     */
    private Object getLogicNotDelVal(TableColumnInfo logicDelColumnInfo) {
        String logicNotDelVal = logicDelColumnInfo.getLogicNotDelVal();
        Class<?> fieldType = logicDelColumnInfo.getField().getType();

        return this.convertValue(logicNotDelVal, fieldType);
    }

    private Object convertValue(String val, Class<?> fieldType) {
        if (Integer.class.equals(fieldType) || int.class.equals(fieldType)) {
            return Integer.valueOf(val);
        } else if (Byte.class.equals(fieldType) || byte.class.equals(fieldType)) {
            return Byte.valueOf(val);
        } else if (Boolean.class.equals(fieldType) || boolean.class.equals(fieldType)) {
            if (trueValues.contains(val)) {
                return 1;
            }
            else if (falseValues.contains(val)) {
                return 0;
            }
        }

        return val;
    }

}
