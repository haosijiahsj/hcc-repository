package com.hcc.repository.starter.autoconfigure;

import com.hcc.repository.core.mapper.BaseMapper;
import com.hcc.repository.core.spring.DS;
import com.hcc.repository.core.spring.support.MapperFactoryBean;
import com.hcc.repository.core.utils.CollUtils;
import com.hcc.repository.core.utils.ReflectUtils;
import com.hcc.repository.core.utils.ScanUtils;
import com.hcc.repository.core.utils.StrUtils;
import lombok.Setter;
import org.springframework.beans.BeansException;
import org.springframework.beans.factory.config.BeanFactoryPostProcessor;
import org.springframework.beans.factory.config.ConfigurableListableBeanFactory;
import org.springframework.beans.factory.config.RuntimeBeanReference;
import org.springframework.beans.factory.support.DefaultListableBeanFactory;
import org.springframework.beans.factory.support.GenericBeanDefinition;

import javax.sql.DataSource;
import java.util.Collection;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * 这个会导致数据源提前加载不出来
 *
 * @author hushengjun
 * @date 2023/4/6
 */
@Deprecated
public class MapperBeanFactoryPostProcessor implements BeanFactoryPostProcessor {

    private final RepositoryProperties properties;
    @Setter
    private DataSource defaultDataSource;

    public MapperBeanFactoryPostProcessor(RepositoryProperties properties) {
        this.properties = properties;
    }

    @Override
    public void postProcessBeanFactory(ConfigurableListableBeanFactory beanFactory) throws BeansException {
        List<String> basePackages = properties.getBasePackages();
        if (CollUtils.isEmpty(basePackages)) {
            throw new RuntimeException("必须指定mapper包路径");
        }
        Set<Class<?>> mapperClasses = basePackages.stream()
                .map(pk -> ScanUtils.scanClass(pk, true))
                .flatMap(Collection::stream)
                .filter(BaseMapper.class::isAssignableFrom)
                .collect(Collectors.toSet());

        mapperClasses.forEach(mapperClass -> {
            String beanName = mapperClass.getName()
                    + "#"
                    + MapperBeanFactoryPostProcessor.class.getSimpleName()
                    + "#"
                    + 0;

            GenericBeanDefinition beanDefinition = new GenericBeanDefinition();
            beanDefinition.getConstructorArgumentValues().addGenericArgumentValue(mapperClass.getName());
            beanDefinition.setBeanClass(MapperFactoryBean.class);

            DS ds = ReflectUtils.getAnnotation(mapperClass, DS.class);
            if (ds != null && StrUtils.isNotEmpty(ds.value())) {
                beanDefinition.getPropertyValues().add("dataSource", new RuntimeBeanReference(ds.value()));
            } else {
                beanDefinition.getPropertyValues().add("dataSource", defaultDataSource);
            }

            ((DefaultListableBeanFactory) beanFactory).registerBeanDefinition(beanName, beanDefinition);
        });

        if (properties.isEnableBanner()) {
            System.out.println("==========================");
            System.out.println("======hcc-repository======");
            System.out.println("==========================");
        }
    }

}
