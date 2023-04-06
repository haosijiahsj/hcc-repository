package com.hcc.repository.starter.autoconfigure;

import com.hcc.repository.core.jdbc.JdbcTemplateProxy;
import com.hcc.repository.core.jdbc.JdbcTemplateWrapper;
import com.hcc.repository.core.proxy.JdbcTemplateProxyInvocationHandler;
import com.hcc.repository.core.spring.support.InjectMapperBeanPostProcessor;
import com.hcc.repository.core.utils.CollUtils;
import com.hcc.repository.core.utils.ReflectUtils;
import com.hcc.repository.extension.transaction.TransactionHelper;
import org.springframework.beans.factory.ObjectProvider;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.transaction.PlatformTransactionManager;

import javax.sql.DataSource;

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
    public MapperBeanFactoryPostProcessor mapperBeanFactoryPostProcessor(DataSource dataSource,
                                                                         ObjectProvider<RepositoryInterceptor> repositoryInterceptorObjectProvider,
                                                                         RepositoryProperties properties) {
        MapperBeanFactoryPostProcessor mapperBeanFactoryPostProcessor = new MapperBeanFactoryPostProcessor(properties);
        mapperBeanFactoryPostProcessor.setDefaultDataSource(dataSource);

        return mapperBeanFactoryPostProcessor;
    }

    @Bean
    public JdbcTemplateProxy jdbcTemplateProxy(DataSource dataSource,
                                               ObjectProvider<RepositoryInterceptor> repositoryInterceptorObjectProvider,
                                               RepositoryProperties properties) {
        RepositoryInterceptor repositoryInterceptor = repositoryInterceptorObjectProvider.getIfAvailable(RepositoryInterceptor::new);
        JdbcTemplateProxyInvocationHandler jdbcTemplateProxyInvocationHandler
                = new JdbcTemplateProxyInvocationHandler(new JdbcTemplateWrapper(dataSource), repositoryInterceptor.getInterceptors());

        return ReflectUtils.newProxy(JdbcTemplateProxy.class, jdbcTemplateProxyInvocationHandler);
    }

    @Bean
    @ConditionalOnMissingBean
    public TransactionHelper transactionHelper(PlatformTransactionManager transactionManager) {
        return new TransactionHelper(transactionManager);
    }

}
