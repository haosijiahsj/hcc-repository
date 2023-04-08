package com.hcc.repository.core.jdbc;

import com.hcc.repository.core.utils.CollUtils;
import com.hcc.repository.core.utils.Pair;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.jdbc.core.namedparam.MapSqlParameterSource;
import org.springframework.jdbc.core.namedparam.NamedParameterJdbcTemplate;
import org.springframework.jdbc.support.GeneratedKeyHolder;
import org.springframework.jdbc.support.KeyHolder;

import javax.sql.DataSource;
import java.sql.PreparedStatement;
import java.sql.Statement;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * JdbcTemplateWrapper
 *
 * @author hushengjun
 * @date 2023/3/17
 */
public class JdbcTemplateWrapper implements JdbcTemplateProxy {

    private JdbcTemplate jdbcTemplate;
    private NamedParameterJdbcTemplate namedParameterJdbcTemplate;

    public JdbcTemplateWrapper(DataSource dataSource) {
        this.jdbcTemplate = new JdbcTemplate(dataSource);
        this.namedParameterJdbcTemplate = new NamedParameterJdbcTemplate(dataSource);
    }

    @Override
    public int namedUpdate(String sql, Map<String, ?> paramMap) {
        return namedParameterJdbcTemplate.update(sql, paramMap);
    }

    @Override
    public int[] namedBatchUpdate(String sql, List<Map<String, Object>> paramMaps) {
        return namedParameterJdbcTemplate.batchUpdate(sql, paramMaps.toArray(new HashMap[0]));
    }

    /**
     * 自增主键这样使用
     * @param sql
     * @param paramMap
     * @return
     */
    @Override
    public Pair<Number, Integer> namedUpdateForKey(String sql, Map<String, ?> paramMap) {
        KeyHolder keyHolder = new GeneratedKeyHolder();
        int rows = namedParameterJdbcTemplate.update(sql, new MapSqlParameterSource(paramMap), keyHolder);

        return Pair.of(keyHolder.getKey(), rows);
    }

    @Override
    public List<Map<String, Object>> namedQueryForList(String sql, Map<String, ?> paramMap) {
        return namedParameterJdbcTemplate.queryForList(sql, paramMap);
    }

    @Override
    public <T> List<T> namedQueryForEntityList(String sql, Map<String, ?> paramMap, Class<T> targetClass) {
        return namedParameterJdbcTemplate.query(sql, paramMap, new GeneralRowMapper<>(targetClass));
    }

    @Override
    public <T> T namedQueryForObject(String sql, Map<String, ?> paramMap, Class<T> targetClass) {
        return namedParameterJdbcTemplate.queryForObject(sql, paramMap, targetClass);
    }

    @Override
    public List<Map<String, Object>> queryForList(String sql, Object[] args) {
        return jdbcTemplate.queryForList(sql, args);
    }

    @Override
    public <T> List<T> queryForEntityList(String sql, Object[] args, Class<T> targetClass) {
        return jdbcTemplate.query(sql, args, new GeneralRowMapper<>(targetClass));
    }

    @Override
    public int update(String sql, Object[] args) {
        return jdbcTemplate.update(sql, args);
    }

    @Override
    public Pair<Number, Integer> updateForKey(String sql, Object[] args) {
        KeyHolder keyHolder = new GeneratedKeyHolder();
        int affectRow = jdbcTemplate.update(connection -> {
            PreparedStatement ps = connection.prepareStatement(sql, Statement.RETURN_GENERATED_KEYS);
            for (int i = 0; i < args.length; i++) {
                ps.setObject(i + 1, args[i]);
            }
            return ps;
        }, keyHolder);

        return Pair.of(keyHolder.getKey(), affectRow);
    }

}
