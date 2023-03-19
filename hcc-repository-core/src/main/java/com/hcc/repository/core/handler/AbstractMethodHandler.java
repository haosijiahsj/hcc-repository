package com.hcc.repository.core.handler;

import com.hcc.repository.core.interceptor.Interceptor;
import com.hcc.repository.core.jdbc.JdbcTemplateWrapper;
import org.springframework.jdbc.core.JdbcTemplate;

import java.util.ArrayList;
import java.util.List;

/**
 * 抽象的方法处理器
 *
 * @author hushengjun
 * @date 2023/3/6
 */
public abstract class AbstractMethodHandler {

    protected JdbcTemplateWrapper jdbcTemplateWrapper;
    protected String methodName;
    protected Object[] args;
    protected List<Interceptor> interceptors = new ArrayList<>();

    public final Object handle() throws Exception {
        // 拦截器执行逻辑
        Object result = this.handleMethod();

        return result;
    }

    protected abstract Object handleMethod() throws Exception;

    public void setJdbcTemplateWrapper(JdbcTemplateWrapper jdbcTemplateWrapper) {
        this.jdbcTemplateWrapper = jdbcTemplateWrapper;
    }

    public void setMethodName(String methodName) {
        this.methodName = methodName;
    }

    public void setArgs(Object[] args) {
        this.args = args;
    }

    public void addInterceptors(List<Interceptor> interceptors) {
        this.interceptors.addAll(interceptors);
    }

    public void addInterceptor(Interceptor interceptor) {
        this.interceptors.add(interceptor);
    }

}