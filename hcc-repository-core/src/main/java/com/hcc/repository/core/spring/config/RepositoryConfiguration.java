package com.hcc.repository.core.spring.config;

import com.hcc.repository.core.interceptor.Interceptor;
import lombok.Data;

import java.util.List;
import java.util.Map;

/**
 * 配置类
 *
 * @author hushengjun
 * @date 2023/4/5
 */
@Data
public class RepositoryConfiguration {

    /**
     * 拦截器
     */
    private List<Interceptor> interceptors;
    /**
     * 配置信息
     */
    private Map<String, Object> properties;

}
