package com.hcc.repository.core.interceptor;

import com.hcc.repository.core.conditions.ICondition;
import com.hcc.repository.core.jdbc.JdbcOperations;
import com.hcc.repository.core.spring.config.RepositoryConfiguration;

import java.lang.reflect.Method;

/**
 * 拦截器<br/>
 * 仅能作用于mapper中的相关方法和sql
 *
 * @author hushengjun
 * @date 2023/3/3
 */
public interface Interceptor {

    /**
     * 准备condition前调用
     * @param method
     * @param parameters
     */
    default void beforePrepareCondition(Method method, Object[] parameters) {}

    /**
     * 准备condition后调用
     * @param method
     * @param parameters
     * @param condition
     */
    default void afterPrepareCondition(Method method, Object[] parameters, ICondition<?> condition) {}

    /**
     * 执行语句之前
     * @param method
     * @param parameters
     * @param jdbcOperations
     * @param context
     */
    default void beforeExecute(Method method, Object[] parameters, JdbcOperations jdbcOperations, SqlExecuteContext context) {}

    /**
     * 执行语句之后
     * @param method
     * @param parameters
     * @param jdbcOperations
     * @param context
     * @param result
     * @return
     */
    default Object afterExecute(Method method, Object[] parameters, JdbcOperations jdbcOperations, SqlExecuteContext context, Object result) {
        return result;
    }

    /**
     * 返回结果之前
     * @param method
     * @param context
     * @param result
     * @return
     */
    default Object beforeReturn(Method method, Object[] parameters, SqlExecuteContext context, Object result) {
        return result;
    }

    /**
     * 自定配置或获取配置
     * @param configuration
     */
    default void customizeConfiguration(RepositoryConfiguration configuration) {}

}
