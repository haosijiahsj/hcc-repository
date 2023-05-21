package com.hcc.repository.core.handler.annotation;

import com.hcc.repository.core.annotation.Condition;
import com.hcc.repository.core.annotation.Conditions;
import com.hcc.repository.core.annotation.Param;
import com.hcc.repository.core.annotation.Query;
import com.hcc.repository.core.conditions.ICondition;
import com.hcc.repository.core.conditions.original.OriginalSqlCondition;
import com.hcc.repository.core.constants.SqlKeywordEnum;
import com.hcc.repository.core.constants.SqlTypeEnum;
import com.hcc.repository.core.constants.StrPool;
import com.hcc.repository.core.exceptions.RepositoryException;
import com.hcc.repository.core.handler.AbstractMethodHandler;
import com.hcc.repository.core.jdbc.ResultMapper;
import com.hcc.repository.core.page.IPage;
import com.hcc.repository.core.utils.ArrayUtils;
import com.hcc.repository.core.utils.Assert;
import com.hcc.repository.core.utils.CollUtils;
import com.hcc.repository.core.utils.ExpressionParseUtils;
import com.hcc.repository.core.utils.Pair;
import com.hcc.repository.core.utils.ReflectUtils;
import com.hcc.repository.core.utils.JSqlParserUtils;
import com.hcc.repository.core.utils.SqlParseUtils;
import com.hcc.repository.core.utils.StrUtils;
import lombok.extern.slf4j.Slf4j;
import org.springframework.core.ResolvableType;

import java.lang.annotation.Annotation;
import java.lang.reflect.Constructor;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * 查询注解处理器
 *
 * @author hushengjun
 * @date 2023/4/28
 */
@Slf4j
public class QueryAnnotationMethodHandler extends AbstractMethodHandler {

    protected final Query queryAnnotation;

    public QueryAnnotationMethodHandler(Query queryAnnotation) {
        this.queryAnnotation = queryAnnotation;
    }

    @Override
    protected void prepare() {
        Assert.isTrue(StrUtils.isNotEmpty(queryAnnotation.value()), "sql不能为空");
        SqlTypeEnum sqlType = JSqlParserUtils.getSqlType(queryAnnotation.value());
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
        Conditions conditions = method.getAnnotation(Conditions.class);
        if (conditions != null && existCondition != null) {
            throw new RepositoryException("@Conditions注解存在，不能再使用ICondition参数");
        }

        Map<String, Object> paramMap = this.collectParam();

        String sql = queryAnnotation.value();
        String expressionSql = this.processCondition(sql, paramMap);

        // 解析表达式sql
        Pair<String, Object[]> pair = SqlParseUtils.parseExpressionSql(expressionSql, paramMap);

        OriginalSqlCondition<?> condition = new OriginalSqlCondition<>();
        condition.sql(pair.getLeft());
        condition.addArg(pair.getRight());

        // 设置sql参数
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
     * 收集参数到Map中
     * @return
     */
    private Map<String, Object> collectParam() {
        if (ArrayUtils.isEmpty(args)) {
            return Collections.emptyMap();
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
                    String paramName = ((Param) argAnnotation).value();
                    if (StrUtils.isEmpty(paramName)) {
                        throw new IllegalArgumentException(String.format("方法：%s，参数列表第%s个参数@Param注解在需要指定value",
                                method.getDeclaringClass().getName() + "." + method.getName(), i + 1));
                    }
                    paramMap.put(paramName, args[i]);
                }
            }
        }

        return paramMap;
    }

    /**
     * 处理condition注解
     * @param mainSql
     * @param paramMap
     * @return
     */
    private String processCondition(String mainSql, Map<String, Object> paramMap) {
        Conditions conditions = method.getAnnotation(Conditions.class);
        if (conditions == null || ArrayUtils.isEmpty(conditions.value())) {
            return mainSql;
        }
        List<String> conditionSegments = new ArrayList<>();
        for (Condition c : conditions.value()) {
            if (StrUtils.isEmpty(c.value())) {
                continue;
            }
            if (StrUtils.isEmpty(c.exp())) {
                conditionSegments.add(c.value());
                continue;
            }
            boolean parseResult = ExpressionParseUtils.assertExpression(c.exp(), paramMap);
            if (parseResult) {
                if (log.isDebugEnabled()) {
                    log.debug("表达式：{} 解析为true", c.exp());
                }
                conditionSegments.add(c.value());
            } else {
                if (log.isDebugEnabled()) {
                    log.debug("表达式：{} 解析为false", c.exp());
                }
            }
        }
        if (CollUtils.isEmpty(conditionSegments)) {
            return mainSql;
        }

        String first = conditionSegments.get(0).toUpperCase();

        // 主sql含有where，直接拼接
        if (mainSql.toUpperCase().contains(SqlKeywordEnum.WHERE.getKeyword())) {
            return mainSql.trim()
                    + StrPool.SPACE
                    + String.join(StrPool.SPACE, conditionSegments);
        }

        // 自动生成where的，去掉第一条件的and或or
        if (first.startsWith(SqlKeywordEnum.AND.getKeyword())
                || first.startsWith(SqlKeywordEnum.OR.getKeyword())) {
            // 第一sql含and 或 or，去掉第一个and或or
            String tempFirst = conditionSegments.get(0).replace(SqlKeywordEnum.AND.getKeyword(), "");
            tempFirst = tempFirst.replace(SqlKeywordEnum.AND.getKeyword().toLowerCase(), "");
            tempFirst = tempFirst.replace(SqlKeywordEnum.OR.getKeyword(), "");
            tempFirst = tempFirst.replace(SqlKeywordEnum.OR.getKeyword().toLowerCase(), "");
            conditionSegments.remove(0);
            conditionSegments.add(0, tempFirst.trim());
        }

        return mainSql.trim()
                + StrPool.SPACE
                + SqlKeywordEnum.WHERE.getKeyword()
                + StrPool.SPACE
                + String.join(StrPool.SPACE, conditionSegments);
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
