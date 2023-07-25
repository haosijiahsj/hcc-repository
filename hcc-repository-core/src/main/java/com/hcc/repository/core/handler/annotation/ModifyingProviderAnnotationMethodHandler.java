package com.hcc.repository.core.handler.annotation;

import com.hcc.repository.core.annotation.ModifyingProvider;
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

    private final ModifyingProvider modifyingProvider;

    public ModifyingProviderAnnotationMethodHandler(QueryProvider queryProvider, ModifyingProvider modifyingProvider) {
        super(queryProvider);
        this.modifyingProvider = modifyingProvider;
    }

    @Override
    protected Object executeSql(String sql, Object[] args) {
        return jdbcOperations.update(sql, args);
    }

}
