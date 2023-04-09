package com.hcc.repository.test.config;

import com.hcc.repository.core.spring.MapperScan;
import com.hcc.repository.extension.interceptor.DynamicTableNameInterceptor;
import com.hcc.repository.extension.interceptor.LogInterceptor;
import com.hcc.repository.extension.interceptor.LogicDeleteInterceptor;
import com.hcc.repository.starter.autoconfigure.RepositoryInterceptor;
import com.hcc.repository.test.dao.interceptor.TestInterceptor;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 * HccRepositoryConfig
 *
 * @author hushengjun
 * @date 2023/4/6
 */
@Configuration
@MapperScan("com.hcc.repository.test.dao.mapper")
public class HccRepositoryConfig {

    @Bean
    public RepositoryInterceptor repositoryInterceptor() {
        RepositoryInterceptor interceptor = new RepositoryInterceptor();
        interceptor.addInterceptor(new TestInterceptor());
        interceptor.addInterceptor(new LogicDeleteInterceptor());
        interceptor.addInterceptor(new DynamicTableNameInterceptor(((originalTableName, curSql) -> originalTableName)));
        interceptor.addInterceptor(new LogInterceptor());

        return interceptor;
    }

}
