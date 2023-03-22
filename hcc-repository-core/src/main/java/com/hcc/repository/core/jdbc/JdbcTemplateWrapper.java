package com.hcc.repository.core.jdbc;

import com.hcc.repository.core.utils.CollUtils;
import com.hcc.repository.core.utils.Pair;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.jdbc.core.namedparam.MapSqlParameterSource;
import org.springframework.jdbc.support.GeneratedKeyHolder;
import org.springframework.jdbc.support.KeyHolder;

import javax.sql.DataSource;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * JdbcTemplateWrapper
 *
 * @author hushengjun
 * @date 2023/3/17
 */
public class JdbcTemplateWrapper {

    private JdbcTemplate jdbcTemplate;
    private NamedParameterJdbcTemplateDelegate namedParameterJdbcTemplateDelegate;

    public JdbcTemplateWrapper(JdbcTemplate jdbcTemplate) {
        this.jdbcTemplate = jdbcTemplate;
        this.namedParameterJdbcTemplateDelegate = new NamedParameterJdbcTemplateDelegate(jdbcTemplate);
    }

    public JdbcTemplateWrapper(DataSource dataSource) {
        this.jdbcTemplate = new JdbcTemplate(dataSource);
        this.namedParameterJdbcTemplateDelegate = new NamedParameterJdbcTemplateDelegate(dataSource);
    }

    public List<Map<String, Object>> namedQueryForList(String sql, Map<String, ?> paramMap) {
        return namedParameterJdbcTemplateDelegate.queryForList(sql, paramMap);
    }

    public int namedUpdate(String sql, Map<String, ?> paramMap) {
        return namedParameterJdbcTemplateDelegate.update(sql, paramMap);
    }

    public int[] namedBatchUpdate(String sql, List<Map<String, Object>> paramMaps) {
        return namedParameterJdbcTemplateDelegate.batchUpdate(sql, paramMaps.toArray(new HashMap[0]));
    }

    /**
     * 自增主键这样使用
     * @param sql
     * @param paramMap
     * @return
     */
    public Pair<Number, Integer> namedUpdateForKey(String sql, Map<String, ?> paramMap) {
        KeyHolder keyHolder = new GeneratedKeyHolder();
        int rows = namedParameterJdbcTemplateDelegate.update(sql, new MapSqlParameterSource(paramMap), keyHolder);

        return Pair.of(keyHolder.getKey(), rows);
    }

    public <T> List<T> namedQueryForList(String sql, Map<String, ?> paramMap, Class<T> targetClass) {
        return namedParameterJdbcTemplateDelegate.query(sql, paramMap, new GeneralRowMapper<>(targetClass));
    }

    public <T> T namedQueryForObject(String sql, Map<String, ?> paramMap, Class<T> targetClass) {
        List<T> results = namedParameterJdbcTemplateDelegate.query(sql, paramMap, new GeneralRowMapper<>(targetClass));
        if (CollUtils.isEmpty(results)) {
            return null;
        }
        if (results.size() > 1) {
            throw new RuntimeException("结果不唯一");
        }

        return results.get(0);
    }

    public <T> List<T> querForList(String sql, Object[] args, Class<T> targetClass) {
        return jdbcTemplate.query(sql, args, new GeneralRowMapper<>(targetClass));
    }

}
