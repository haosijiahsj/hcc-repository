package com.hcc.repository.starter.autoconfigure;

import com.hcc.repository.core.jdbc.JdbcOperations;
import com.hcc.repository.core.jdbc.JdbcOperationsImpl;
import com.hcc.repository.core.proxy.JdbcOperationsInvocationHandler;
import com.hcc.repository.core.spring.config.RepositoryConfiguration;
import com.hcc.repository.core.spring.support.InjectMapperBeanPostProcessor;
import com.hcc.repository.core.spring.support.MapperBeanDefinitionRegistryPostProcessor;
import com.hcc.repository.core.spring.support.MapperFactoryBean;
import com.hcc.repository.core.spring.support.MapperScanBeanRegistrar;
import com.hcc.repository.core.utils.ReflectUtils;
import com.hcc.repository.core.utils.StrUtils;
import com.hcc.repository.extension.transaction.TransactionHelper;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.BeansException;
import org.springframework.beans.factory.BeanFactory;
import org.springframework.beans.factory.BeanFactoryAware;
import org.springframework.beans.factory.InitializingBean;
import org.springframework.beans.factory.ObjectProvider;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.support.BeanDefinitionBuilder;
import org.springframework.beans.factory.support.BeanDefinitionRegistry;
import org.springframework.boot.autoconfigure.AutoConfigurationPackages;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Import;
import org.springframework.context.annotation.ImportBeanDefinitionRegistrar;
import org.springframework.core.type.AnnotationMetadata;
import org.springframework.transaction.PlatformTransactionManager;

import javax.sql.DataSource;
import java.util.List;

/**
 * 自动配置类
 *
 * @author hushengjun
 * @date 2023/4/5
 */
@Slf4j
@Configuration
@EnableConfigurationProperties(RepositoryProperties.class)
public class RepositoryAutoConfiguration {

    @Autowired
    private RepositoryProperties properties;

    @Bean
    @ConditionalOnMissingBean
    public RepositoryConfiguration repositoryConfiguration(ObjectProvider<RepositoryInterceptor> repositoryInterceptorObjectProvider) {
        RepositoryInterceptor repositoryInterceptor = repositoryInterceptorObjectProvider.getIfAvailable(RepositoryInterceptor::new);
        RepositoryConfiguration configuration = new RepositoryConfiguration();
        configuration.setInterceptors(repositoryInterceptor.getInterceptors());
        configuration.setPrintSqlLog(properties.isPrintSqlLog());
        configuration.setProperties(properties.getProperties());

        return configuration;
    }

    @Bean
    @ConditionalOnMissingBean
    public InjectMapperBeanPostProcessor injectMapperBeanPostProcessor(DataSource dataSource, RepositoryConfiguration configuration) {
        InjectMapperBeanPostProcessor beanPostProcessor = new InjectMapperBeanPostProcessor(dataSource);
        beanPostProcessor.setConfiguration(configuration);

        return beanPostProcessor;
    }

    @Bean
    public JdbcOperations jdbcOperations(DataSource dataSource, RepositoryConfiguration configuration) {
        JdbcOperationsInvocationHandler jdbcOperationsInvocationHandler
                = new JdbcOperationsInvocationHandler(new JdbcOperationsImpl(dataSource), configuration);

        return ReflectUtils.newProxy(JdbcOperations.class, jdbcOperationsInvocationHandler);
    }

    @Bean
    @ConditionalOnMissingBean
    public TransactionHelper transactionHelper(PlatformTransactionManager transactionManager) {
        return new TransactionHelper(transactionManager);
    }

    @Configuration
    @Import(AutoConfigMapperBeanRegistrar.class)
    @ConditionalOnMissingBean({MapperFactoryBean.class, MapperBeanDefinitionRegistryPostProcessor.class})
    public static class AutoConfigMapperConfiguration implements InitializingBean {

        @Autowired
        private RepositoryProperties properties;

        @Override
        public void afterPropertiesSet() throws Exception {}

    }

    /**
     * Mapper bean注册器
     */
    public static class AutoConfigMapperBeanRegistrar implements BeanFactoryAware, ImportBeanDefinitionRegistrar {

        private BeanFactory beanFactory;

        @Override
        public void setBeanFactory(BeanFactory beanFactory) throws BeansException {
            this.beanFactory = beanFactory;
        }

        @Override
        public void registerBeanDefinitions(AnnotationMetadata importingClassMetadata, BeanDefinitionRegistry registry) {
            if (!AutoConfigurationPackages.has(beanFactory)) {
                return;
            }

            List<String> basePackages = AutoConfigurationPackages.get(beanFactory);
            BeanDefinitionBuilder builder = BeanDefinitionBuilder.genericBeanDefinition(MapperBeanDefinitionRegistryPostProcessor.class);
            // 向下的传递的注解属性相关信息
            builder.addPropertyValue("basePackage", StrUtils.join(",", basePackages));

            // 向容器注入bean
            registry.registerBeanDefinition(MapperScanBeanRegistrar.class.getName(), builder.getBeanDefinition());
        }

    }

}
