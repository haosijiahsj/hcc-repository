package com.hcc.repository.core.spring.support;

import com.hcc.repository.core.exceptions.RepositoryException;
import com.hcc.repository.core.mapper.BaseMapper;
import com.hcc.repository.core.spring.DS;
import com.hcc.repository.core.utils.CollUtils;
import com.hcc.repository.core.utils.ReflectUtils;
import com.hcc.repository.core.utils.StrUtils;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.AnnotatedBeanDefinition;
import org.springframework.beans.factory.config.BeanDefinitionHolder;
import org.springframework.beans.factory.config.RuntimeBeanReference;
import org.springframework.beans.factory.support.AbstractBeanDefinition;
import org.springframework.beans.factory.support.BeanDefinitionRegistry;
import org.springframework.beans.factory.support.GenericBeanDefinition;
import org.springframework.context.annotation.ClassPathBeanDefinitionScanner;

import java.util.Arrays;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * 扫描器，扫描mapper
 *
 * @author hushengjun
 * @date 2023/4/5
 */
@Slf4j
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
        if (CollUtils.isEmpty(holders)) {
            throw new RepositoryException(String.format("Could not find mapper in '%s'", Arrays.toString(basePackages)));
        }

        // 往这个里面塞入MapperBeanFactory
        this.processBeanDefinitions(holders);

        return holders;
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

            // 加入动态数据源功能 扫描DS注解获取dataSource依赖名称
            Class<?> mapperBeanClass = ReflectUtils.forName(beanClassName);
            DS ds = ReflectUtils.getAnnotation(mapperBeanClass, DS.class);
            if (ds != null && StrUtils.isNotEmpty(ds.value())) {
                if (log.isDebugEnabled()) {
                    log.debug("mapper {} 使用了DS注解指定数据源bean为：{}", beanClassName, ds.value());
                }
                dataSourceRef = ds.value();
            }

//            boolean autoWiredDataSource = true;
            if (StrUtils.isNotEmpty(dataSourceRef)) {
                definition.getPropertyValues().add("dataSource", new RuntimeBeanReference(dataSourceRef));
//                autoWiredDataSource = false;
            }
            if (StrUtils.isNotEmpty(configurationRef)) {
                definition.getPropertyValues().add("configuration", new RuntimeBeanReference(configurationRef));
            }
            // 自动注入数据源依赖，DynamicDaoFactoryBean继承的setDataSource方法会自动执行
//            if (autoWiredDataSource) {
//                definition.setAutowireMode(AbstractBeanDefinition.AUTOWIRE_BY_TYPE);
//            }
            definition.setAutowireMode(AbstractBeanDefinition.AUTOWIRE_BY_TYPE);
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
        // exclude package-info.java 以及不是继承自BaseMapper的bean
        addExcludeFilter((metadataReader, metadataReaderFactory) -> {
            String className = metadataReader.getClassMetadata().getClassName();
            Class<?> beanClass = ReflectUtils.forName(className);

            return className.endsWith("package-info") || !BaseMapper.class.isAssignableFrom(beanClass);
        });
    }

}
