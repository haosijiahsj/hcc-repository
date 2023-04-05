package com.hcc.repository.core.jdbc;

import com.hcc.repository.core.exceptions.RepositoryException;
import com.hcc.repository.core.utils.CollUtils;
import com.hcc.repository.core.utils.Pair;

import java.util.List;
import java.util.Map;

/**
 * JdbcTemplateProxy
 *
 * @author hushengjun
 * @date 2023/3/26
 */
public interface JdbcTemplateProxy {

    int namedUpdate(String sql, Map<String, ?> paramMap);

    int[] namedBatchUpdate(String sql, List<Map<String, Object>> paramMaps);

    Pair<Number, Integer> namedUpdateForKey(String sql, Map<String, ?> paramMap);

    List<Map<String, Object>> namedQueryForList(String sql, Map<String, ?> paramMap);

    <T> List<T> namedQueryForEntityList(String sql, Map<String, ?> paramMap, Class<T> entityClass);

    default <T> T namedQueryForEntityObj(String sql, Map<String, ?> paramMap, Class<T> entityClass) {
        List<T> results = namedQueryForEntityList(sql, paramMap, entityClass);
        if (CollUtils.isEmpty(results)) {
            return null;
        }
        if (results.size() > 1) {
            throw new RepositoryException("结果不唯一");
        }

        return results.get(0);
    }

    <T> T namedQueryForObject(String sql, Map<String, ?> paramMap, Class<T> targetClass);

    List<Map<String, Object>> queryForList(String sql, Object[] args);

    <T> List<T> queryForEntityList(String sql, Object[] args, Class<T> entityClass);

    default  <T> T queryForEntityObj(String sql, Object[] args, Class<T> entityClass) {
        List<T> results = this.queryForEntityList(sql, args, entityClass);
        if (CollUtils.isEmpty(results)) {
            return null;
        }
        if (results.size() > 1) {
            throw new RepositoryException("结果不唯一");
        }

        return results.get(0);
    }

    int update(String sql, Object[] args);

    Pair<Number, Integer> updateForKey(String sql, Object[] args);

}
