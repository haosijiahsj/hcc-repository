package com.hcc.repository.core.handler.annotation;

import com.hcc.repository.core.annotation.Condition;
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
import com.hcc.repository.core.utils.JSqlParserUtils;
import com.hcc.repository.core.utils.ReflectUtils;
import com.hcc.repository.core.utils.StrUtils;
import lombok.extern.slf4j.Slf4j;
import org.springframework.core.ResolvableType;

import java.lang.annotation.Annotation;
import java.lang.reflect.Constructor;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
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

        Condition[] conditions = queryAnnotation.conditions();
        if (ArrayUtils.isNotEmpty(conditions) && existCondition != null) {
            throw new RepositoryException("@Condition注解存在，不能再使用ICondition参数");
        }

        // 收集参数
        Map<String, Object> paramMap = this.collectParam();

        // 处理sql
        String mainSql = queryAnnotation.value();
        mainSql = this.processJoinCondition(mainSql, paramMap);
        String placeholderSql = this.processCondition(mainSql, paramMap) + StrPool.SPACE + queryAnnotation.last();

        // 构建信息
        OriginalSqlCondition<?> condition = new OriginalSqlCondition<>();
        condition.sql(placeholderSql.trim());
        condition.putParamMap(paramMap);

        // 设置sql参数
        if (existCondition != null) {
            // 需要拼接用户sql
            String sqlAfterWhere = existCondition.getSqlAfterWhere();
            if (StrUtils.isNotEmpty(sqlAfterWhere)) {
                condition.sql(mainSql + StrPool.SPACE + sqlAfterWhere);
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
     * 处理join的Condition
     * @param mainSql
     * @param paramMap
     * @return
     */
    private String processJoinCondition(String mainSql, Map<String, Object> paramMap) {
        Condition[] joinConditions = queryAnnotation.joinConditions();
        if (ArrayUtils.isEmpty(joinConditions)) {
            return mainSql;
        }
        List<String> conditionSegments = this.calcConditionExpression(joinConditions, paramMap);
        if (CollUtils.isEmpty(conditionSegments)) {
            return mainSql;
        }

        return StrUtils.join(StrPool.SPACE, conditionSegments);
    }

    /**
     * 处理condition注解
     * @param mainSql
     * @param paramMap
     * @return
     */
    private String processCondition(String mainSql, Map<String, Object> paramMap) {
        Condition[] conditions = queryAnnotation.conditions();
        if (ArrayUtils.isEmpty(conditions)) {
            return mainSql;
        }
        List<String> conditionSegments = this.calcConditionExpression(conditions, paramMap);
        if (CollUtils.isEmpty(conditionSegments)) {
            return mainSql;
        }

        // 第一个sql片段
        String firstUpperCaseSegment = conditionSegments.get(0).toUpperCase();

        boolean needConcatWhere = this.needConcatWhere(mainSql, firstUpperCaseSegment);
        if (!needConcatWhere) {
            return mainSql.trim()
                    + StrPool.SPACE
                    + String.join(StrPool.SPACE, conditionSegments);
        }

        // 这里自动生成where，去掉第一条件的and或or
        if (firstUpperCaseSegment.startsWith(SqlKeywordEnum.AND.getKeyword())
                || firstUpperCaseSegment.startsWith(SqlKeywordEnum.OR.getKeyword())) {
            // 第一sql含and 或 or，去掉第一个and或or
            String tempFirst = conditionSegments.get(0).replaceFirst("(?i)AND|(?i)OR", StrPool.EMPTY);
            conditionSegments.remove(0);
            conditionSegments.add(0, tempFirst.trim());
        }

        return StrUtils.join(StrPool.SPACE,
                mainSql.trim(),
                SqlKeywordEnum.WHERE.getKeyword(),
                String.join(StrPool.SPACE, conditionSegments));
    }

    /**
     * 计算表达式
     * @param conditions
     * @param paramMap
     * @return
     */
    private List<String> calcConditionExpression(Condition[] conditions, Map<String, Object> paramMap) {
        List<String> conditionSegments = new ArrayList<>();
        for (Condition c : conditions) {
            String expression = c.exp().trim();
            String sqlSegment = c.value().trim();
            if (StrUtils.isEmpty(sqlSegment)) {
                continue;
            }
            if (StrUtils.isEmpty(expression)) {
                conditionSegments.add(sqlSegment.trim());
                continue;
            }
            boolean parseResult = ExpressionParseUtils.assertExpression(expression, paramMap);
            if (parseResult) {
                if (log.isDebugEnabled()) {
                    log.debug("表达式：{} 解析为true", expression);
                }
                conditionSegments.add(sqlSegment.trim());
            } else {
                if (log.isDebugEnabled()) {
                    log.debug("表达式：{} 解析为false", expression);
                }
            }
        }

        return conditionSegments;
    }

    /**
     * 是否要拼接where
     * @param mainSql
     * @param firstSegment
     * @return
     */
    private boolean needConcatWhere(String mainSql, String firstSegment) {
        // 主sql含where的和第一个Condition sql不拼接where
        if (mainSql.matches("(?i)WHERE")
                || firstSegment.matches("(?i)WHERE")) {
            return false;
        }
        if (firstSegment.startsWith(SqlKeywordEnum.GROUP_BY.getKeyword())
                || firstSegment.startsWith(SqlKeywordEnum.ORDER_BY.getKeyword())) {
            return false;
        }

        return true;
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
