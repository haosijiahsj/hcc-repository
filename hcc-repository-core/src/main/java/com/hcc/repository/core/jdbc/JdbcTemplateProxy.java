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

    /**
     * 具名更新
     * @param sql
     * @param paramMap
     * @return
     */
    int namedUpdate(String sql, Map<String, ?> paramMap);

    /**
     * 批量具名更新
     * @param sql
     * @param paramMaps
     * @return
     */
    int[] namedBatchUpdate(String sql, List<Map<String, Object>> paramMaps);

    /**
     * 具名更新获取key
     * @param sql
     * @param paramMap
     * @return
     */
    Pair<Number, Integer> namedUpdateForKey(String sql, Map<String, ?> paramMap);

    /**
     * 具名查询map列表
     * @param sql
     * @param paramMap
     * @return
     */
    List<Map<String, Object>> namedQueryForList(String sql, Map<String, ?> paramMap);

    /**
     * 具名查询实体列表
     * @param sql
     * @param paramMap
     * @param entityClass
     * @param <T>
     * @return
     */
    <T> List<T> namedQueryForEntityList(String sql, Map<String, ?> paramMap, Class<T> entityClass);

    /**
     * 具名查询实体
     * @param sql
     * @param paramMap
     * @param entityClass
     * @param <T>
     * @return
     */
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

    /**
     * 具名查询对象列表
     * @param sql
     * @param paramMap
     * @param targetClass
     * @param <T>
     * @return
     */
    <T> T namedQueryForObject(String sql, Map<String, ?> paramMap, Class<T> targetClass);

    /**
     * 查询map列表
     * @param sql
     * @param args
     * @return
     */
    List<Map<String, Object>> queryForList(String sql, Object[] args);

    /**
     * 查询实体列表
     * @param sql
     * @param args
     * @param entityClass
     * @param <T>
     * @return
     */
    <T> List<T> queryForEntityList(String sql, Object[] args, Class<T> entityClass);

    /**
     * 查询实体对象
     * @param sql
     * @param args
     * @param entityClass
     * @param <T>
     * @return
     */
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

    /**
     * 更新
     * @param sql
     * @param args
     * @return
     */
    int update(String sql, Object[] args);

    /**
     * 更新获取key
     * @param sql
     * @param args
     * @return
     */
    Pair<Number, Integer> updateForKey(String sql, Object[] args);

}
