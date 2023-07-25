package com.hcc.repository.core.handler.annotation;

import com.hcc.repository.core.annotation.Modifying;
import com.hcc.repository.core.annotation.QueryProvider;
import lombok.extern.slf4j.Slf4j;

/**
 * 查询注解处理器
 *
 * @author hushengjun
 * @date 2023/4/28
 */
@Slf4j
public class ModifyingProviderAnnotationMethodHandler extends QueryProviderAnnotationMethodHandler {

    private final Modifying modifyingAnnotation;

    public ModifyingProviderAnnotationMethodHandler(QueryProvider queryProvider, Modifying modifyingAnnotation) {
        super(queryProvider);
        this.modifyingAnnotation = modifyingAnnotation;
    }

    @Override
    protected Object executeSql(String sql, Object[] args) {
        return jdbcOperations.update(sql, args);
    }

}
