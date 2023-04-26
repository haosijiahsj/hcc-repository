package com.hcc.repository.core.interceptor;

import com.hcc.repository.core.conditions.ICondition;
import com.hcc.repository.core.constants.MethodNameEnum;
import com.hcc.repository.core.jdbc.JdbcOperations;

/**
 * 拦截器
 *
 * @author hushengjun
 * @date 2023/3/3
 */
public interface Interceptor {

    /**
     * 准备condition前调用
     * @param methodNameEnum
     * @param args
     */
    default void beforePrepareCondition(MethodNameEnum methodNameEnum, Object[] args) {}

    /**
     * 准备condition后调用
     * @param methodNameEnum
     * @param args
     * @param condition
     */
    default void afterPrepareCondition(MethodNameEnum methodNameEnum, Object[] args, ICondition<?> condition) {}

    /**
     * 执行语句之前
     * @param jdbcOperations
     * @param context
     */
    default void beforeExecute(JdbcOperations jdbcOperations, SqlExecuteContext context) {}

    /**
     * 返回之前调用
     * @param jdbcOperations
     * @param context
     * @param result
     * @return
     */
    default Object beforeReturn(JdbcOperations jdbcOperations, SqlExecuteContext context, Object result) {
        return result;
    }

}
