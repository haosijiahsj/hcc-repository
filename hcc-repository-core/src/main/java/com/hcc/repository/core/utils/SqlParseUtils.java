package com.hcc.repository.core.utils;

import com.hcc.repository.core.constants.StrPool;
import org.springframework.jdbc.core.namedparam.MapSqlParameterSource;
import org.springframework.jdbc.core.namedparam.NamedParameterUtils;
import org.springframework.jdbc.core.namedparam.ParsedSql;

import java.lang.reflect.Array;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * @author hushengjun
 * @date 2019/9/20
 */
public class SqlParseUtils {

    private static final Pattern SQL_PLACEHOLDER_PATTERN = Pattern.compile("[#|$]\\{(.+?)}");

    private SqlParseUtils() {}

    /**
     * 解析具名参数sql, 返回?占位符的sql与参数数组
     * @param namedSql
     * @param paramMap
     * @return
     */
    public static Pair<String, Object[]> parseNamedSql(String namedSql, Map<String, Object> paramMap) {
        ParsedSql parsedSql = NamedParameterUtils.parseSqlStatement(namedSql);

        String sqlToUse = NamedParameterUtils.substituteNamedParameters(parsedSql, new MapSqlParameterSource(paramMap));
        Object[] originalArgs = NamedParameterUtils.buildValueArray(parsedSql, new MapSqlParameterSource(paramMap), null);

        List<Object> argsList = new ArrayList<>();
        for (Object originalArg : originalArgs) {
            // 展开参数
            if (originalArg instanceof Collection) {
                argsList.addAll((Collection<?>) originalArg);
            } else if (originalArg != null && originalArg.getClass().isArray()) {
                argsList.addAll(Arrays.asList(originalArgs));
            } else {
                argsList.add(originalArg);
            }
        }

        return Pair.of(sqlToUse, argsList.toArray());
    }

    /**
     * 解析表达式sql
     * @param expressionSql
     * @param paramMap
     * @return
     */
    public static Pair<String, Object[]> parsePlaceholderSql(String expressionSql, Map<String, Object> paramMap) {
        Map<String, Object> namedParamMap = new HashMap<>();
        Map<String, String> replacePlaceholderMap = new HashMap<>();
        Map<String, Object> replaceValueMap = new HashMap<>();

        Matcher matcher = SQL_PLACEHOLDER_PATTERN.matcher(expressionSql);
        while (matcher.find()) {
            String exp = matcher.group(0);
            String content = matcher.group(1);
            Object result = ExpressionParseUtils.parsePlaceholder(exp, paramMap);
            if (exp.startsWith(StrPool.HASH)) {
                namedParamMap.put(content, result);
                replacePlaceholderMap.put(exp, content);
            } else if (exp.startsWith(StrPool.DOLLAR)) {
                replaceValueMap.put(exp, result);
            }
        }

        // 将#{}占位符替换为jdbcTemplate具名形式 #{a} -> :a
        for (Map.Entry<String, String> entry : replacePlaceholderMap.entrySet()) {
            String k = entry.getKey();
            String v = entry.getValue();
            expressionSql = expressionSql.replace(k, StrPool.COLON + v);
        }
        // 将${}占位符替换为实际值 ${a} -> a
        for (Map.Entry<String, Object> entry : replaceValueMap.entrySet()) {
            String k = entry.getKey();
            Object v = entry.getValue();

            expressionSql = expressionSql.replace(k, getSqlValue(v));
        }

        return parseNamedSql(expressionSql, namedParamMap);
    }

    /**
     * 获取sql值，根据值类型来拼接值到sql
     * @param value
     * @return
     */
    private static String getSqlValue(Object value) {
        if (value == null) {
            return "null";
        }
        if (value instanceof String
                || value instanceof LocalDateTime
                || value instanceof LocalDate) {
            return String.format("'%s'", value);
        }

        if (value instanceof Date) {
            return String.valueOf(((Date) value).getTime());
        }

        // byte处理
        if (Byte.class.equals(value.getClass())) {
            return String.format("b'%s'", value);
        }

        // 展开列表
        if (value instanceof Collection) {
            return ((Collection<?>) value).stream()
                    .map(SqlParseUtils::getSqlValue)
                    .collect(Collectors.joining(StrPool.COMMA_SPACE));
        }

        // 展开数组
        if (value.getClass().isArray()) {
            return Stream.of((Array) value)
                    .map(SqlParseUtils::getSqlValue)
                    .collect(Collectors.joining(StrPool.COMMA_SPACE));
        }

        return value.toString();
    }

}
