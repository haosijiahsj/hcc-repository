package com.hcc.repository.core.utils;

import org.springframework.jdbc.core.namedparam.MapSqlParameterSource;
import org.springframework.jdbc.core.namedparam.NamedParameterUtils;
import org.springframework.jdbc.core.namedparam.ParsedSql;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Map;

/**
 * @author hushengjun
 * @date 2019/9/20
 */
public class SqlParseUtils {

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

    public static Pair<String, Object[]> parsePlaceholderSql(String namedSql, Map<String, Object> paramMap) {
        return null;
    }

}
