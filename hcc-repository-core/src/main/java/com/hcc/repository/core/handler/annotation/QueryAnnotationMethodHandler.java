package com.hcc.repository.core.handler.annotation;

import com.hcc.repository.core.annotation.Param;
import com.hcc.repository.core.annotation.Query;
import com.hcc.repository.core.conditions.ICondition;
import com.hcc.repository.core.conditions.original.OriginalSqlCondition;
import com.hcc.repository.core.constants.SqlTypeEnum;
import com.hcc.repository.core.handler.AbstractMethodHandler;
import com.hcc.repository.core.jdbc.ResultMapper;
import com.hcc.repository.core.page.IPage;
import com.hcc.repository.core.utils.ArrayUtils;
import com.hcc.repository.core.utils.Assert;
import com.hcc.repository.core.utils.CollUtils;
import com.hcc.repository.core.utils.ReflectUtils;
import com.hcc.repository.core.utils.SqlParserUtils;
import com.hcc.repository.core.utils.StrUtils;
import org.springframework.core.ResolvableType;

import java.lang.annotation.Annotation;
import java.lang.reflect.Constructor;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * 查询注解处理器
 *
 * @author hushengjun
 * @date 2023/4/28
 */
public class QueryAnnotationMethodHandler extends AbstractMethodHandler {

    protected final Query queryAnnotation;

    public QueryAnnotationMethodHandler(Query queryAnnotation) {
        this.queryAnnotation = queryAnnotation;
    }

    @Override
    protected void prepare() {
        Assert.isTrue(StrUtils.isNotEmpty(queryAnnotation.value()), "sql不能为空");
        SqlTypeEnum sqlType = SqlParserUtils.getSqlType(queryAnnotation.value());
        Assert.isTrue(SqlTypeEnum.SELECT.equals(sqlType), "非select语句");
    }

    @Override
    protected ICondition<?> prepareCondition() {
        ICondition<?> existCondition = null;
        for (Object arg : args) {
            if (arg instanceof ICondition) {
                existCondition = (ICondition<?>) arg;
            }
        }
        String sql = queryAnnotation.value();
        OriginalSqlCondition<?> condition = new OriginalSqlCondition<>();
        condition.sql(sql);

        // 设置sql参数
        this.setConditionParam(condition);
        if (existCondition != null) {
            // 需要拼接用户sql
            String sqlAfterWhere = existCondition.getSqlAfterWhere();
            if (StrUtils.isNotEmpty(sqlAfterWhere)) {
                condition.sql(sql + " " + sqlAfterWhere);
                condition.putParamMap(existCondition.getColumnValuePairs());
            }
        }

        return condition;
    }

    /**
     * 设置condition参数
     * @param condition
     */
    private void setConditionParam(OriginalSqlCondition<?> condition) {
        if (ArrayUtils.isEmpty(args)) {
            return;
        }
        // 参数上的注解
        Annotation[][] methodParaAnnotations = method.getParameterAnnotations();

        Map<String, Object> paramMap = new HashMap<>();
        for (int i = 0; i < args.length; i++) {
            if (args[i] instanceof ICondition || args[i] instanceof IPage) {
                continue;
            }
            Annotation[] argAnnotations = methodParaAnnotations[i];
            for (Annotation argAnnotation : argAnnotations) {
                if (Param.class.equals(argAnnotation.annotationType())) {
                    if (args[i] instanceof Map) {
                        paramMap.putAll((Map<String, ?>) args[i]);
                    } else {
                        String parameterName = ((Param) argAnnotation).value();
                        if (StrUtils.isEmpty(parameterName)) {
                            throw new IllegalArgumentException(String.format("方法：%s，参数列表第%s个参数@Param注解在非Map类型的情况下需要指定value",
                                    method.getDeclaringClass().getName() + "." + method.getName(), i + 1));
                        }
                        paramMap.put(parameterName, args[i]);
                    }
                }
            }
        }

        // 具名方式
        if (CollUtils.isNotEmpty(paramMap)) {
            condition.putParamMap(paramMap);
            return;
        }

        // 普通方式，需要排除ICondition和IPage类型，这两个无法直接作为sql参数
        for (Object arg : args) {
            if (arg instanceof ICondition || arg instanceof IPage) {
                continue;
            }
            condition.addArg(arg);
        }
    }

    @Override
    protected Object executeSql(String sql, Object[] args) {
        Class<?> genericClass = method.getReturnType();
        if (Collection.class.isAssignableFrom(genericClass)
                || IPage.class.isAssignableFrom(genericClass)) {
            ResolvableType resolvableType = ResolvableType.forMethodReturnType(method);
            genericClass = resolvableType.getGeneric(0).resolve();
        }
        ResultMapper<?> rowMapper = this.newInstanceRowMapper(queryAnnotation.resultMapper(), genericClass);

        if (method.getReturnType().equals(genericClass)) {
            // 返回值为对象
            return jdbcOperations.queryForObject(sql, args, rowMapper);
        }
        // 返回值为集合
        List<?> results = jdbcOperations.queryForList(sql, args, rowMapper);
        if (Set.class.isAssignableFrom(method.getReturnType())) {
            return new HashSet<>(results);
        }

        return results;
    }

    /**
     * 实例化BasicRowMapper
     * @param rowMapperClass
     * @return
     */
    private ResultMapper<?> newInstanceRowMapper(Class<? extends ResultMapper> rowMapperClass, Class<?> targetClass) {
        Constructor<?>[] constructors = rowMapperClass.getDeclaredConstructors();
        Assert.isTrue(constructors.length >= 1, String.format("%s 无构造方法", rowMapperClass.getName()));

        ResultMapper<?> resultMapper = null;
        for (Constructor<?> constructor : constructors) {
            int parameterCount = constructor.getParameterCount();
            if (parameterCount == 1) {
                try {
                    resultMapper = (ResultMapper<?>) constructor.newInstance(targetClass);
                    break;
                } catch (Exception e) {
                    throw new RuntimeException(e);
                }
            }
        }

        // 默认返回无参的
        if (resultMapper == null) {
            resultMapper = ReflectUtils.newInstance(rowMapperClass);
        }

        return resultMapper;
    }

}
