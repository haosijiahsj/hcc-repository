package com.hcc.repository.core.handler.annotation;

import com.hcc.repository.core.annotation.Condition;
import com.hcc.repository.core.annotation.Param;
import com.hcc.repository.core.annotation.Query;
import com.hcc.repository.core.conditions.ICondition;
import com.hcc.repository.core.conditions.nativesql.NativeSqlCondition;
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
import com.hcc.repository.core.utils.MethodParamNameUtils;
import com.hcc.repository.core.utils.ReflectUtils;
import com.hcc.repository.core.utils.StrUtils;
import lombok.extern.slf4j.Slf4j;
import org.springframework.core.ResolvableType;

import java.lang.annotation.Annotation;
import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;

/**
 * 查询注解处理器
 *
 * @author hushengjun
 * @date 2023/4/28
 */
@Slf4j
public class QueryAnnotationMethodHandler extends AbstractMethodHandler {

    private static final String AND_OR_REGEX = "(?i)AND|(?i)OR";
    private static final String SET_VAL_REGEX_COMMA = ".+=.+,";
    private static final String SET_VAL_REGEX = ".+=.+";
    protected Query queryAnnotation;
    protected Class<? extends ResultMapper> resultMapper;

    public QueryAnnotationMethodHandler(Query queryAnnotation) {
        this.queryAnnotation = queryAnnotation;
        this.resultMapper = queryAnnotation.resultMapper();
    }

    public QueryAnnotationMethodHandler(Class<? extends ResultMapper> resultMapper) {
        this.resultMapper = resultMapper;
    }

    @Override
    protected void prepare() {
        Assert.isTrue(StrUtils.isNotEmpty(queryAnnotation.value()), "sql不能为空");
        SqlTypeEnum sqlType = JSqlParserUtils.getSqlTypeFromPrefix(queryAnnotation.value());
        Assert.isTrue(SqlTypeEnum.SELECT.equals(sqlType), "非select语句");
    }

    @Override
    protected ICondition<?> prepareCondition() {
        ICondition<?> existCondition = null;
        int conditionParamCount = 0;
        for (Object arg : args) {
            if (arg instanceof ICondition) {
                existCondition = (ICondition<?>) arg;
                conditionParamCount++;
            }
        }
        if (existCondition != null && conditionParamCount > 1) {
            throw new RepositoryException("ICondition参数只能存在一次");
        }

        // Condition注解和ICondition参数不能共存
        Condition[] conditions = queryAnnotation.conditions();
        if (ArrayUtils.isNotEmpty(conditions) && existCondition != null) {
            throw new RepositoryException("Condition注解存在，不能再使用ICondition参数");
        }

        // 收集参数
        Map<String, Object> paramMap = this.collectParam();

        // 处理sql
        String mainSql = queryAnnotation.value();
        String placeholderSql = this.processCondition(mainSql, paramMap)
                + StrPool.SPACE
                + queryAnnotation.last();

        // 构建Condition
        NativeSqlCondition<?> condition = new NativeSqlCondition<>();
        condition.sql(placeholderSql.trim());
        condition.putParamMap(paramMap);

        // 拼接Condition中的条件
        if (existCondition != null) {
            // 需要拼接用户sql
            String sqlAfterWhere = existCondition.getSqlAfterWhere();
            if (StrUtils.isNotEmpty(sqlAfterWhere)) {
                String tempMainSql = mainSql.toUpperCase().trim();
                if (tempMainSql.contains(SqlKeywordEnum.WHERE.getKeyword())) {
                    // 主sql含有where则去掉Condition中的where
                    sqlAfterWhere = sqlAfterWhere.replace(SqlKeywordEnum.WHERE.getKeyword(), StrPool.EMPTY).trim();
                    if (!tempMainSql.endsWith(SqlKeywordEnum.WHERE.getKeyword())) {
                        // 不是where结尾的拼接and关键字
                        sqlAfterWhere = SqlKeywordEnum.AND.getKeyword() + StrPool.SPACE + sqlAfterWhere;
                    }
                }
                String finalSql = mainSql + StrPool.SPACE + sqlAfterWhere + StrPool.SPACE + queryAnnotation.last();
                condition.sql(finalSql.trim());
                condition.putParamMap(existCondition.getColumnValuePairs());
            }
        }

        return condition;
    }

    /**
     * 收集参数到Map中
     * @return
     */
    protected Map<String, Object> collectParam() {
        if (ArrayUtils.isEmpty(args)) {
            return Collections.emptyMap();
        }
        // 尝试获取方法参数名称
        String[] methodParamNames = MethodParamNameUtils.getMethodParamNames(method);
        // 参数上的注解
        Annotation[][] methodParaAnnotations = method.getParameterAnnotations();

        Map<String, Object> paramMap = new HashMap<>();
        for (int i = 0; i < args.length; i++) {
            // 支持通过位置来使用参数无需标注Param注解，但实际开发不建议
            paramMap.put("arg" + i, args[i]);
            if (args[i] instanceof ICondition || args[i] instanceof IPage) {
                // 这两个类型参与Param解析
                continue;
            }
            Annotation[] argAnnotations = methodParaAnnotations[i];
            for (Annotation argAnnotation : argAnnotations) {
                if (Param.class.equals(argAnnotation.annotationType())) {
                    String paramName = ((Param) argAnnotation).value();
                    if (StrUtils.isEmpty(paramName)) {
                        if (ArrayUtils.isEmpty(methodParamNames)) {
                            throw new IllegalArgumentException(String.format("编译时未使用-parameters参数，方法：%s，参数列表第%s个参数@Param注解需要设置value",
                                    method.getDeclaringClass().getName() + "." + method.getName(), i + 1));
                        }
                        paramName = methodParamNames[i];
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
        Condition[] conditions = queryAnnotation.conditions();
        if (ArrayUtils.isEmpty(conditions)) {
            return mainSql;
        }
        List<String> conditionSegments = this.calcConditionExpression(conditions, paramMap);
        if (CollUtils.isEmpty(conditionSegments)
                || (conditionSegments.size() == 1
                && SqlKeywordEnum.WHERE.getKeyword().equalsIgnoreCase(conditionSegments.get(0)))) {
            // Condition中的sql为空，或只有一个where的不拼接
            return mainSql;
        }

        StringBuilder mainSqlBuilder = new StringBuilder(mainSql);
        for (int i = 0; i < conditionSegments.size(); i++) {
            String segment = conditionSegments.get(i);
            String tempMainSql = mainSqlBuilder.toString().trim().toUpperCase();
            if (tempMainSql.endsWith(SqlKeywordEnum.WHERE.getKeyword())) {
                // 每次拼接后都判断是否由WHERE结尾
                String tempSegment = segment.toUpperCase();
                if (tempSegment.startsWith(SqlKeywordEnum.AND.getKeyword())
                        || tempSegment.startsWith(SqlKeywordEnum.OR.getKeyword())) {
                    // 若当前拼接的sql是AND或OR开头则此sql片段去掉AND或OR
                    if (log.isDebugEnabled()) {
                        log.debug("sql片段：{}将去除AND或OR", segment);
                    }
                    segment = segment.replaceFirst(AND_OR_REGEX, StrPool.EMPTY).trim();
                }
            }

            // 如果当前sql含SET关键字，且当前语句以逗号结尾
            if (tempMainSql.contains(SqlKeywordEnum.SET.getKeyword()) && segment.endsWith(StrPool.COMMA)
                    && segment.matches(SET_VAL_REGEX_COMMA)) {
                if (i == conditionSegments.size() - 1) {
                    // 当前语句为最后一个，则去掉末尾的逗号
                    segment = segment.substring(0, segment.length() - 1);
                } else if (i < conditionSegments.size() - 1) {
                    String nextSegment = conditionSegments.get(i + 1);
                    if (!nextSegment.endsWith(StrPool.COMMA) && !nextSegment.matches(SET_VAL_REGEX)) {
                        // 后一个语句不是逗号结尾，且不是赋值语句，则当前语句去掉末尾逗号
                        segment = segment.substring(0, segment.length() - 1);
                        if (log.isDebugEnabled()) {
                            log.debug("sql片段：{}将去除(,)", segment);
                        }
                    }
                }
            }

            // 最后拼接此sql片段
            mainSqlBuilder.append(StrPool.SPACE).append(segment);
        }

        return mainSqlBuilder.toString();
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

    @Override
    protected Object executeSql(String sql, Object[] args) {
        Class<?> genericClass = method.getReturnType();
        if (Collection.class.isAssignableFrom(genericClass)
                || IPage.class.isAssignableFrom(genericClass)) {
            ResolvableType resolvableType = ResolvableType.forMethodReturnType(method);
            genericClass = resolvableType.getGeneric(0).resolve();
        }
        ResultMapper<?> rowMapper = this.newInstanceRowMapper(resultMapper, genericClass);

        if (method.getReturnType().equals(genericClass)) {
            // 返回值为对象
            return jdbcOperations.queryForObject(sql, args, rowMapper);
        }
        // 返回值为集合
        List<?> results = jdbcOperations.queryForList(sql, args, rowMapper);
        if (Set.class.equals(method.getReturnType())) {
            return new HashSet<>(results);
        }

        return results;
    }

    /**
     * 实例化BasicRowMapper
     * @param rowMapperClass
     * @return
     */
    @SuppressWarnings("unchecked")
    private ResultMapper<?> newInstanceRowMapper(Class<? extends ResultMapper> rowMapperClass, Class<?> targetClass) {
        return Optional.ofNullable(ReflectUtils.matchConstruct(rowMapperClass, Class.class))
                .map(c -> {
                    try {
                        return (ResultMapper<?>) c.newInstance(targetClass);
                    } catch (InstantiationException | IllegalAccessException | InvocationTargetException e) {
                        throw new RuntimeException(e);
                    }
                })
                .orElseGet(() -> ReflectUtils.newInstance(rowMapperClass));
    }

}
