package com.hcc.repository.core.jdbc;

import com.hcc.repository.core.exceptions.RepositoryException;
import com.hcc.repository.core.exceptions.TooManyResultException;
import com.hcc.repository.core.jdbc.batch.PreparedStatementObjectSetter;
import com.hcc.repository.core.jdbc.mapper.MapResultMapper;
import com.hcc.repository.core.jdbc.mapper.ObjectResultMapper;
import com.hcc.repository.core.jdbc.mapper.RepoEntityResultMapper;
import com.hcc.repository.core.utils.CollUtils;
import com.hcc.repository.core.utils.Pair;
import org.springframework.jdbc.core.BatchPreparedStatementSetter;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.jdbc.core.namedparam.MapSqlParameterSource;
import org.springframework.jdbc.core.namedparam.NamedParameterJdbcTemplate;
import org.springframework.jdbc.support.GeneratedKeyHolder;
import org.springframework.jdbc.support.KeyHolder;

import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * jdbc操作类，封装jdbcTemplate和namedParameterJdbcTemplate
 *
 * @author hushengjun
 * @date 2023/3/26
 */
public interface JdbcOperations {

    /**
     * 获取原生jdbcTemplate
     * @return
     */
    JdbcTemplate getJdbcTemplate();

    /**
     * 获取原生namedParameterJdbcTemplate
     * @return
     */
    NamedParameterJdbcTemplate getNamedParameterJdbcTemplate();

    /**
     * 具名更新
     * @param sql
     * @param paramMap
     * @return
     */
    default int namedUpdate(String sql, Map<String, Object> paramMap) {
        return getNamedParameterJdbcTemplate().update(sql, paramMap);
    }

    /**
     * 批量具名更新
     * @param sql
     * @param paramMaps
     * @return
     */
    default int[] namedBatchUpdate(String sql, List<Map<String, Object>> paramMaps) {
        return getNamedParameterJdbcTemplate().batchUpdate(sql, paramMaps.toArray(new HashMap[0]));
    }

    /**
     * 具名更新获取key
     * @param sql
     * @param paramMap
     * @return
     */
    default Pair<Number, Integer> namedUpdateForKey(String sql, Map<String, Object> paramMap) {
        KeyHolder keyHolder = new GeneratedKeyHolder();
        int rows = getNamedParameterJdbcTemplate().update(sql, new MapSqlParameterSource(paramMap), keyHolder);

        return Pair.of(keyHolder.getKey(), rows);
    }

    /**
     * 具名查询map列表
     * @param sql
     * @param paramMap
     * @return
     */
    default List<Map<String, Object>> namedQueryForList(String sql, Map<String, Object> paramMap) {
        return getNamedParameterJdbcTemplate().query(sql, paramMap, RowMapperWrapper.create(new MapResultMapper()));
    }

    /**
     * 具名查询实体列表
     * @param sql
     * @param paramMap
     * @param entityClass
     * @param <T>
     * @return
     */
    default <T> List<T> namedQueryForEntityList(String sql, Map<String, Object> paramMap, Class<T> entityClass) {
        return getNamedParameterJdbcTemplate().query(sql, paramMap, RowMapperWrapper.create(new RepoEntityResultMapper<>(entityClass)));
    }

    /**
     * 具名查询实体
     * @param sql
     * @param paramMap
     * @param entityClass
     * @param <T>
     * @return
     */
    default <T> T namedQueryForEntityObj(String sql, Map<String, Object> paramMap, Class<T> entityClass) {
        List<T> results = namedQueryForEntityList(sql, paramMap, entityClass);
        if (CollUtils.isEmpty(results)) {
            return null;
        }
        if (results.size() > 1) {
            throw new RepositoryException(String.format("预期一条数据，实际%s条数据", results.size()));
        }

        return results.get(0);
    }

    /**
     * 更新
     * @param sql
     * @param args
     * @return
     */
    default int update(String sql, Object[] args) {
        return getJdbcTemplate().update(sql, args);
    }

    /**
     * 批量处理
     * @param sql
     * @param batchArgs
     * @param setter
     * @param <T>
     * @return
     */
    default <T> int[] batchUpdate(String sql, List<T> batchArgs, PreparedStatementObjectSetter<T> setter) {
        return getJdbcTemplate().batchUpdate(sql, new BatchPreparedStatementSetter() {
            @Override
            public void setValues(PreparedStatement ps, int i) throws SQLException {
                T object = batchArgs.get(i);
                if (object == null) {
                    return;
                }
                setter.setValues(ps, object);
            }

            @Override
            public int getBatchSize() {
                return batchArgs == null ? 0 : batchArgs.size();
            }
        });
    }

    /**
     * 更新获取key
     * @param sql
     * @param args
     * @return
     */
    default Pair<Number, Integer> updateForKey(String sql, Object[] args) {
        KeyHolder keyHolder = new GeneratedKeyHolder();
        int affectRow = getJdbcTemplate().update(connection -> {
            PreparedStatement ps = connection.prepareStatement(sql, Statement.RETURN_GENERATED_KEYS);
            for (int i = 0; i < args.length; i++) {
                ps.setObject(i + 1, args[i]);
            }
            return ps;
        }, keyHolder);

        return Pair.of(keyHolder.getKey(), affectRow);
    }

    /**
     * 查询map列表
     * @param sql
     * @param args
     * @return
     */
    default List<Map<String, Object>> queryForList(String sql, Object[] args) {
        return getJdbcTemplate().queryForList(sql, args);
    }

    /**
     * 自定义映射器查询列表
     * @param sql
     * @param args
     * @param rowMapper
     * @return
     * @param <T>
     */
    default <T> List<T> queryForList(String sql, Object[] args, ResultMapper<T> rowMapper) {
        return getJdbcTemplate().query(sql, args, RowMapperWrapper.create(rowMapper));
    }

    /**
     * 查询实体列表
     * @param sql
     * @param args
     * @param entityClass
     * @param <T>
     * @return
     */
    default <T> List<T> queryForEntityList(String sql, Object[] args, Class<T> entityClass) {
        return getJdbcTemplate().query(sql, args, RowMapperWrapper.create(new RepoEntityResultMapper<>(entityClass)));
    }

    /**
     * 自定义映射器查询对象
     * @param sql
     * @param args
     * @param resultMapper
     * @return
     * @param <T>
     */
    default <T> T queryForObject(String sql, Object[] args, ResultMapper<T> resultMapper) {
        List<T> results = queryForList(sql, args, resultMapper);
        if (CollUtils.isEmpty(results)) {
            return null;
        }
        if (results.size() > 1) {
            throw new TooManyResultException(1, results.size());
        }

        return results.get(0);
    }

    /**
     * 查询对象数据
     * @param sql
     * @param args
     * @param requiredType
     * @param <T>
     * @return
     */
    default <T> T queryForObject(String sql, Object[] args, Class<T> requiredType) {
        return getJdbcTemplate().queryForObject(sql, args, RowMapperWrapper.create(new ObjectResultMapper<>(requiredType)));
    }

    /**
     * 查询实体对象
     * @param sql
     * @param args
     * @param entityClass
     * @param <T>
     * @return
     */
    default <T> T queryForEntityObj(String sql, Object[] args, Class<T> entityClass) {
        List<T> results = this.queryForEntityList(sql, args, entityClass);
        if (CollUtils.isEmpty(results)) {
            return null;
        }
        if (results.size() > 1) {
            throw new TooManyResultException(1, results.size());
        }

        return results.get(0);
    }

}
