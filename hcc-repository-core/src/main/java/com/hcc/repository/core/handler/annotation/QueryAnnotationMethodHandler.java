package com.hcc.repository.core.handler.annotation;

import com.hcc.repository.core.annotation.Query;
import com.hcc.repository.core.conditions.ICondition;
import com.hcc.repository.core.conditions.OriginalSqlCondition;
import com.hcc.repository.core.handler.AbstractMethodHandler;
import com.hcc.repository.core.jdbc.BasicRowMapper;
import com.hcc.repository.core.utils.ReflectUtils;
import org.springframework.core.ResolvableType;

/**
 * 查询注解处理器
 *
 * @author hushengjun
 * @date 2023/4/28
 */
public class QueryAnnotationMethodHandler extends AbstractMethodHandler {

    private final Query queryAnnotation;
    private final Class<?> rowMapperClass;

    public QueryAnnotationMethodHandler(Query queryAnnotation) {
        this.queryAnnotation = queryAnnotation;
        this.rowMapperClass = queryAnnotation.rowMapper();
    }

    @Override
    protected ICondition<?> prepareCondition() {
        OriginalSqlCondition<?> condition = new OriginalSqlCondition<>();
        condition.sql(queryAnnotation.value());

        // TODO 设置sql参数

        return condition;
    }

    @Override
    protected Object executeSql(String sql, Object[] args) {
        BasicRowMapper<?> rowMapper = (BasicRowMapper<?>) ReflectUtils.newInstance(rowMapperClass);
        ResolvableType resolvableType = ResolvableType.forMethodReturnType(method);
        Class<?> resolve = resolvableType.getGeneric(0).resolve();

        if (resolve == null) {
            return jdbcOperations.queryForObject(sql, args, rowMapper);
        }
        return jdbcOperations.query(sql, args, rowMapper);
    }

}
