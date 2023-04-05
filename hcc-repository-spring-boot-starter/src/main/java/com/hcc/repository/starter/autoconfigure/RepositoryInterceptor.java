package com.hcc.repository.starter.autoconfigure;

import com.hcc.repository.core.interceptor.Interceptor;

import java.util.ArrayList;
import java.util.List;

/**
 * 注入拦截器的bean
 *
 * @author hushengjun
 * @date 2023/4/5
 */
public class RepositoryInterceptor {

    private final List<Interceptor> interceptors;

    public RepositoryInterceptor() {
        interceptors = new ArrayList<>();
    }

    public void addInterceptor(Interceptor interceptor) {
        interceptors.add(interceptor);
    }

    public List<Interceptor> getInterceptors() {
        return interceptors;
    }
}
