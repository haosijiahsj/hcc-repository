package com.hcc.repository.starter.autoconfigure;

import com.hcc.repository.core.interceptor.Interceptor;
import com.hcc.repository.core.jdbc.JdbcTemplateProxy;
import com.hcc.repository.core.jdbc.JdbcTemplateWrapper;
import com.hcc.repository.core.proxy.JdbcTemplateProxyInvocationHandler;
import com.hcc.repository.core.spring.support.InjectMapperBeanPostProcessor;
import com.hcc.repository.core.utils.ReflectUtils;
import com.hcc.repository.extension.transaction.TransactionHelper;
import org.springframework.beans.factory.ObjectProvider;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.transaction.PlatformTransactionManager;

import javax.sql.DataSource;
import java.util.ArrayList;
import java.util.List;

/**
 * 自动配置类
 *
 * @author hushengjun
 * @date 2023/4/5
 */
@Configuration
public class RepositoryAutoConfiguration {

    @Bean
    public InjectMapperBeanPostProcessor injectMapperBeanPostProcessor(DataSource dataSource) {
        return new InjectMapperBeanPostProcessor(dataSource);
    }

    @Bean
    public JdbcTemplateProxy jdbcTemplateProxy(DataSource dataSource,
                                               ObjectProvider<RepositoryInterceptor> repositoryInterceptorObjectProvider,
                                               RepositoryProperties properties) {
        RepositoryInterceptor repositoryInterceptor = repositoryInterceptorObjectProvider.getIfAvailable();
        List<Interceptor> interceptors = new ArrayList<>();
        if (repositoryInterceptor != null) {
            interceptors = repositoryInterceptor.getInterceptors();
        }
        JdbcTemplateProxyInvocationHandler jdbcTemplateProxyInvocationHandler
                = new JdbcTemplateProxyInvocationHandler(new JdbcTemplateWrapper(dataSource), interceptors);

        return ReflectUtils.newProxy(JdbcTemplateProxy.class, jdbcTemplateProxyInvocationHandler);
    }

    @Bean
    @ConditionalOnMissingBean
    public TransactionHelper transactionHelper(PlatformTransactionManager transactionManager) {
        return new TransactionHelper(transactionManager);
    }

}
