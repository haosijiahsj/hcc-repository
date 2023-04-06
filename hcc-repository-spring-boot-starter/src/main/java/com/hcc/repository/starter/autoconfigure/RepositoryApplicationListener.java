package com.hcc.repository.starter.autoconfigure;

import com.hcc.repository.core.metadata.TableInfoHelper;
import com.hcc.repository.core.utils.CollUtils;
import com.hcc.repository.core.utils.ScanUtils;
import org.springframework.beans.BeansException;
import org.springframework.boot.context.event.ApplicationReadyEvent;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationContextAware;
import org.springframework.context.ApplicationListener;

import java.util.Collection;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * RepositoryApplicationListener
 *
 * @author hushengjun
 * @date 2023/4/6
 */
public class RepositoryApplicationListener implements ApplicationListener<ApplicationReadyEvent>, ApplicationContextAware {

    private ApplicationContext applicationContext;

    @Override
    public void onApplicationEvent(ApplicationReadyEvent event) {
        RepositoryProperties properties = applicationContext.getBean(RepositoryProperties.class);
        if (CollUtils.isNotEmpty(properties.getEntityPackages())) {
            Set<Class<?>> entityClasses = properties.getEntityPackages().stream()
                    .map(pk -> ScanUtils.scanClass(pk, true))
                    .flatMap(Collection::stream)
                    .collect(Collectors.toSet());
            TableInfoHelper.loadAll(entityClasses);
        }
    }

    @Override
    public void setApplicationContext(ApplicationContext applicationContext) throws BeansException {
        this.applicationContext = applicationContext;
    }

}
