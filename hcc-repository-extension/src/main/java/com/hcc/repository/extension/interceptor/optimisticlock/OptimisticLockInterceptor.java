package com.hcc.repository.extension.interceptor.optimisticlock;

import com.hcc.repository.core.conditions.ICondition;
import com.hcc.repository.core.conditions.update.AbstractUpdateCondition;
import com.hcc.repository.core.constants.MethodNameEnum;
import com.hcc.repository.core.constants.StrPool;
import com.hcc.repository.core.interceptor.Interceptor;
import com.hcc.repository.core.metadata.TableColumnInfo;
import com.hcc.repository.core.metadata.TableInfoHelper;
import com.hcc.repository.core.utils.ReflectUtils;
import com.hcc.repository.core.utils.StrUtils;
import lombok.extern.slf4j.Slf4j;

import java.lang.reflect.Method;
import java.sql.Timestamp;
import java.time.LocalDateTime;
import java.util.Date;

/**
 * 乐观锁拦截器，仅支持updateById和updateEntity方法
 *
 * @author hushengjun
 * @date 2023/5/3
 */
@Slf4j
public class OptimisticLockInterceptor implements Interceptor {

    private static final String versionColumnName = "HCC_OPT_LOCK_VERSION_COLUMN";

    @Override
    public void afterPrepareCondition(Method method, Object[] parameters, ICondition<?> condition) {
        MethodNameEnum methodNameEnum = MethodNameEnum.get(method.getName());
        if (!MethodNameEnum.UPDATE_BY_ID.equals(methodNameEnum)
                && !MethodNameEnum.UPDATE_ENTITY.equals(methodNameEnum)) {
            if (log.isDebugEnabled()) {
                log.debug("当前方法：{}不支持乐观锁处理", method.getName());
            }
            return;
        }
        if (!(condition instanceof AbstractUpdateCondition)) {
            return;
        }
        TableColumnInfo versionColumnInfo = TableInfoHelper.getVersionColumnInfo(condition.getEntityClass());
        if (versionColumnInfo == null) {
            if (log.isDebugEnabled()) {
                log.debug("当前class：{}无乐观锁字段", condition.getEntityClass().getName());
            }
            return;
        }
        Object originalVersionVal = getOriginalVersionVal(parameters[0], versionColumnInfo);
        if (originalVersionVal == null) {
            log.warn("要使用乐观锁，必须在传入的实体中赋值乐观锁字段：{}原始值", versionColumnInfo.getFieldName());
            return;
        }
        AbstractUpdateCondition<?, ?, ?> updateCondition = (AbstractUpdateCondition<?, ?, ?>) condition;

        String changeColumnName = versionColumnName + "_CHANGE";
        updateCondition.setSql(StrUtils.format("{0} = {1}", versionColumnInfo.getColumnName(), StrPool.getPlaceholder(changeColumnName)));
        updateCondition.putColumnValuePair(changeColumnName, getChangeVersionVal(originalVersionVal, versionColumnInfo.getField().getType()));

        String originalColumnName = versionColumnName + "_ORIGINAL";
        updateCondition.apply(StrUtils.format("AND {0} = {1}", versionColumnInfo.getColumnName(), StrPool.getPlaceholder(originalColumnName)));
        updateCondition.putColumnValuePair(originalColumnName, originalVersionVal);
    }

    /**
     * 原始值
     * @param entity
     * @param columnInfo
     * @return
     */
    private Object getOriginalVersionVal(Object entity, TableColumnInfo columnInfo) {
        if (entity == null) {
            return null;
        }
        return ReflectUtils.getValue(entity, columnInfo.getField());
    }

    /**
     * 新值
     * @param originalVersionVal
     * @param versionFieldClass
     * @return
     */
    private Object getChangeVersionVal(Object originalVersionVal, Class<?> versionFieldClass) {
        if (long.class.equals(versionFieldClass) || Long.class.equals(versionFieldClass)) {
            return ((long) originalVersionVal) + 1;
        } else if (int.class.equals(versionFieldClass) || Integer.class.equals(versionFieldClass)) {
            return ((int) originalVersionVal) + 1;
        } else if (Date.class.equals(versionFieldClass)) {
            return new Date();
        } else if (Timestamp.class.equals(versionFieldClass)) {
            return new Timestamp(System.currentTimeMillis());
        } else if (LocalDateTime.class.equals(versionFieldClass)) {
            return LocalDateTime.now();
        }

        return originalVersionVal;
    }

}
