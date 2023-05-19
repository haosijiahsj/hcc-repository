package com.hcc.repository.core.utils;

import lombok.AllArgsConstructor;
import lombok.Data;
import org.springframework.expression.EvaluationContext;
import org.springframework.expression.ExpressionParser;
import org.springframework.expression.common.TemplateParserContext;
import org.springframework.expression.spel.standard.SpelExpressionParser;
import org.springframework.expression.spel.support.StandardEvaluationContext;

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
public class PlaceholderParseUtils {

    private static final Pattern pattern = Pattern.compile("^[#|$]\\{(.+)}$");

    private PlaceholderParseUtils() {}

    /**
     * #{#t.name}
     * @param placeholder
     * @param paramMap
     * @return
     */
    public static Object parse(String placeholder, Map<String, Object> paramMap, Class<?> clazz) {
        if (StrUtils.isEmpty(placeholder)) {
            return null;
        }
        Matcher matcher = pattern.matcher(placeholder);
        if (!matcher.find()) {
            throw new RuntimeException(String.format("非法表达式：%s", placeholder));
        }
        String ex = "#" + matcher.group(1);

        ExpressionParser parser = new SpelExpressionParser();
        EvaluationContext context = new StandardEvaluationContext();
        if (CollUtils.isNotEmpty(paramMap)) {
            paramMap.forEach(context::setVariable);
        }

        Object result;
        try {
            if (clazz == null) {
                result = parser.parseExpression(ex).getValue(context);
            } else {
                result = parser.parseExpression(ex).getValue(context, clazz);
            }
        } catch (Exception e) {
            throw new RuntimeException("解析表达式出错", e);
        }

        return result;
    }

    public static Object parse(String placeholder, Map<String, Object> paramMap) {
        return parse(placeholder, paramMap, null);
    }

    public static Object parse(String placeholder, String paramName, Object variable) {
        Map<String, Object> paramMap = Collections.singletonMap(paramName, variable);
        return parse(placeholder, paramMap);
    }

//    public static void main(String[] args) {
//        System.out.println(parse("#{a}", "a", 1));
//        System.out.println(parse("${t.sex}", "t", new T(1L, "hsj", false)));
//    }
//
//    @AllArgsConstructor
//    @Data
//    private static class T {
//        private Long id;
//        private String name;
//        private Boolean sex;
//    }

}
