package com.hcc.repository.core.spring.support;

import com.hcc.repository.core.exceptions.RepositoryException;
import com.hcc.repository.core.mapper.BaseMapper;
import com.hcc.repository.core.utils.CollUtils;
import com.hcc.repository.core.utils.StrUtils;
import lombok.Setter;
import org.springframework.beans.factory.annotation.AnnotatedBeanDefinition;
import org.springframework.beans.factory.config.BeanDefinition;
import org.springframework.beans.factory.config.BeanDefinitionHolder;
import org.springframework.beans.factory.config.RuntimeBeanReference;
import org.springframework.beans.factory.support.AbstractBeanDefinition;
import org.springframework.beans.factory.support.BeanDefinitionRegistry;
import org.springframework.beans.factory.support.GenericBeanDefinition;
import org.springframework.context.annotation.ClassPathBeanDefinitionScanner;

import java.util.Arrays;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * 扫描器，扫描mapper
 *
 * @author hushengjun
 * @date 2023/4/5
 */
public class ClassPathMapperScanner extends ClassPathBeanDefinitionScanner {

    @Setter
    private String dataSourceRef;
    @Setter
    private String configurationRef;

    public ClassPathMapperScanner(BeanDefinitionRegistry registry) {
        super(registry);
    }

    @Override
    protected Set<BeanDefinitionHolder> doScan(String... basePackages) {
        Set<BeanDefinitionHolder> holders = super.doScan(basePackages);
        Set<BeanDefinitionHolder> filterHolders = holders.stream()
                .filter(holder -> {
                    GenericBeanDefinition definition = (GenericBeanDefinition) holder.getBeanDefinition();
                    Class<?> beanClass = definition.getBeanClass();

                    return BaseMapper.class.isAssignableFrom(beanClass);
                })
                .collect(Collectors.toSet());
        if (CollUtils.isEmpty(filterHolders)) {
            throw new RepositoryException(String.format("Could not find mapper in '%s'", Arrays.toString(basePackages)));
        }

        // 往这个里面塞入MapperBeanFactory
        this.processBeanDefinitions(filterHolders);

        return filterHolders;
    }

    /**
     * 处理dao bean, MapperDaoFactoryBean注入
     * @param definitionHolders
     */
    private void processBeanDefinitions(Set<BeanDefinitionHolder> definitionHolders) {
        for (BeanDefinitionHolder holder : definitionHolders) {
            GenericBeanDefinition definition = (GenericBeanDefinition) holder.getBeanDefinition();
            String beanClassName = definition.getBeanClassName();
            // 设置mapper的beanName
            definition.getConstructorArgumentValues().addGenericArgumentValue(beanClassName);
            // 通过该FactoryBean.getObject获取代理对象
            definition.setBeanClass(MapperFactoryBean.class);

            boolean autoWiredDataSource = true;
            if (StrUtils.isNotEmpty(dataSourceRef)) {
                definition.getPropertyValues().add("dataSource", new RuntimeBeanReference(dataSourceRef));
                autoWiredDataSource = false;
            }
            if (StrUtils.isNotEmpty(configurationRef)) {
                definition.getPropertyValues().add("configuration", new RuntimeBeanReference(configurationRef));
            }
            // 自动注入数据源依赖，DynamicDaoFactoryBean继承的setDataSource方法会自动执行
            if (autoWiredDataSource) {
                definition.setAutowireMode(AbstractBeanDefinition.AUTOWIRE_BY_TYPE);
            }
        }
    }

    @Override
    protected boolean isCandidateComponent(AnnotatedBeanDefinition beanDefinition) {
        return beanDefinition.getMetadata().isInterface() && beanDefinition.getMetadata().isIndependent();
    }

    /**
     * 注入需要包含和排除的bean
     */
    public void registerFilters() {
        addIncludeFilter((metadataReader, metadataReaderFactory) -> true);
        // exclude package-info.java
        addExcludeFilter((metadataReader, metadataReaderFactory) -> {
            String className = metadataReader.getClassMetadata().getClassName();
            return className.endsWith("package-info");
        });
    }

}
