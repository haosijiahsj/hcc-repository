package com.hcc.repository.core.interceptor;

import com.hcc.repository.core.conditions.ICondition;
import com.hcc.repository.core.constants.MethodNameEnum;
import com.hcc.repository.core.jdbc.JdbcTemplateProxy;

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
     * @param condition
     */
    default void afterPrepareCondition(MethodNameEnum methodNameEnum, ICondition<?> condition) {}

    /**
     * 执行语句之前
     * @param jdbcTemplateProxy
     * @param context
     */
    default void beforeExecute(JdbcTemplateProxy jdbcTemplateProxy, SqlExecuteContext context) {}

    /**
     * 返回之前调用
     * @param jdbcTemplateProxy
     * @param context
     * @param result
     * @return
     */
    default Object beforeReturn(JdbcTemplateProxy jdbcTemplateProxy, SqlExecuteContext context, Object result) {
        return result;
    }

}
