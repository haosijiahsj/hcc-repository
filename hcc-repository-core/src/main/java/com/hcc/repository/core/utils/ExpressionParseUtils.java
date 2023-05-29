package com.hcc.repository.core.utils;

import org.springframework.context.expression.MapAccessor;
import org.springframework.expression.AccessException;
import org.springframework.expression.EvaluationContext;
import org.springframework.expression.ExpressionParser;
import org.springframework.expression.TypedValue;
import org.springframework.expression.spel.standard.SpelExpressionParser;
import org.springframework.expression.spel.support.StandardEvaluationContext;
import org.springframework.util.Assert;

import java.util.Collections;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * 占位符解析工具
 *
 * @author hushengjun
 * @date 2023/5/19
 */
public class ExpressionParseUtils {

    private static final Pattern SQL_PLACEHOLDER_PATTERN = Pattern.compile("^[#|$]\\{(.+?)}$");

    private ExpressionParseUtils() {}

    /**
     * 解析SpEL表达式
     * @param expression
     * @param paramMap
     * @param clazz
     * @return
     */
    public static Object parse(String expression, Map<String, Object> paramMap, Class<?> clazz) {
        ExpressionParser parser = new SpelExpressionParser();
        StandardEvaluationContext context = new StandardEvaluationContext();
        context.addPropertyAccessor(new NullSafeMapAccessor());
        if (CollUtils.isNotEmpty(paramMap)) {
            paramMap.forEach(context::setVariable);
        }

        Object result;
        try {
            if (clazz == null) {
                result = parser.parseExpression(expression).getValue(context);
            } else {
                result = parser.parseExpression(expression).getValue(context, clazz);
            }
        } catch (Exception e) {
            throw new RuntimeException(String.format("解析表达式：%s 出错", expression), e);
        }

        return result;
    }

    /**
     * #{t.name}
     * @param placeholder
     * @param paramMap
     * @return
     */
    public static Object parsePlaceholder(String placeholder, Map<String, Object> paramMap, Class<?> clazz) {
        if (StrUtils.isEmpty(placeholder)) {
            return null;
        }
        Matcher matcher = SQL_PLACEHOLDER_PATTERN.matcher(placeholder);
        if (!matcher.find()) {
            throw new RuntimeException(String.format("非法表达式：%s", placeholder));
        }

        // group(1) 是括号里面的
        String ex = "#" + matcher.group(1);

        return parse(ex, paramMap, clazz);
    }

    /**
     * 解析为对象
     * @param placeholder
     * @param paramMap
     * @return
     */
    public static Object parsePlaceholder(String placeholder, Map<String, Object> paramMap) {
        return parsePlaceholder(placeholder, paramMap, null);
    }

    /**
     * 指定参数解析
     * @param placeholder
     * @param paramName
     * @param variable
     * @return
     */
    public static Object parsePlaceholder(String placeholder, String paramName, Object variable) {
        Map<String, Object> paramMap = Collections.singletonMap(paramName, variable);
        return parsePlaceholder(placeholder, paramMap);
    }

    /**
     * 断言表达式
     * @param expression
     * @param paramMap
     * @return
     */
    public static boolean assertExpression(String expression, Map<String, Object> paramMap) {
        return (boolean) parse(expression, paramMap, Boolean.class);
    }

    /**
     * 指定参数断言
     * @param expression
     * @param paramName
     * @param variable
     * @return
     */
    public static boolean assertExpression(String expression, String paramName, Object variable) {
        Map<String, Object> paramMap = Collections.singletonMap(paramName, variable);
        return (boolean) parse(expression, paramMap, Boolean.class);
    }

    private static class NullSafeMapAccessor extends MapAccessor {
        @Override
        public boolean canRead(EvaluationContext context, Object target, String name) throws AccessException {
            return target instanceof Map;
        }

        @Override
        public TypedValue read(EvaluationContext context, Object target, String name) throws AccessException {
            Assert.state(target instanceof Map, "Target must be of type Map");
            Map<?, ?> map = (Map<?, ?>) target;
            Object value = map.get(name);
            if (value == null) {
                return TypedValue.NULL;
            }
            return new TypedValue(value);
        }
    }

}
