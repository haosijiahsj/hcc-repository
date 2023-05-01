package com.hcc.repository.core.handler.annotation;

import com.hcc.repository.core.annotation.Modifying;
import com.hcc.repository.core.annotation.Query;
import com.hcc.repository.core.constants.SqlTypeEnum;
import com.hcc.repository.core.utils.Assert;
import com.hcc.repository.core.utils.SqlParserUtils;
import com.hcc.repository.core.utils.StrUtils;

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
    protected void prepare() {
        Assert.isTrue(StrUtils.isNotEmpty(queryAnnotation.value()), "sql不能为空");
        SqlTypeEnum sqlType = SqlParserUtils.getSqlType(queryAnnotation.value());
        Assert.isTrue(!SqlTypeEnum.SELECT.equals(sqlType), "非insert、update、delete语句");
    }

    @Override
    protected Object executeSql(String sql, Object[] args) {
        return jdbcOperations.update(sql, args);
    }

}
