package com.hcc.repository.core.handler.annotation;

import com.hcc.repository.core.annotation.Modifying;
import com.hcc.repository.core.annotation.Query;

/**
 * 修改注解处理器
 *
 * @author hushengjun
 * @date 2023/4/28
 */
public class ModifyingAnnotationMethodHandler extends QueryAnnotationMethodHandler {

    private final Modifying modifyingAnnotation;

    public ModifyingAnnotationMethodHandler(Query queryAnnotation, Modifying modifyingAnnotation) {
        super(queryAnnotation);
        this.modifyingAnnotation = modifyingAnnotation;
    }

    @Override
    protected Object executeSql(String sql, Object[] args) {
        return jdbcOperations.update(sql, args);
    }

}
