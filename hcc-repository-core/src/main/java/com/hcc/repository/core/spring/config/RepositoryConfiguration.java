package com.hcc.repository.core.spring.config;

import com.hcc.repository.core.interceptor.Interceptor;
import lombok.Data;

import java.util.List;

/**
 * 配置类
 *
 * @author hushengjun
 * @date 2023/4/5
 */
@Data
public class RepositoryConfiguration {

    private List<Interceptor> interceptors;

}
