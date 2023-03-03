package com.hcc.repository.core.mapper;

import com.hcc.repository.core.condition.Condition;

import java.io.Serializable;
import java.util.Collection;
import java.util.List;
import java.util.Map;

/**
 * 基础mapper，提供基本的crud功能
 *
 * @author hushengjun
 * @date 2023/3/3
 */
public interface BaseMapper<T, ID extends Serializable> {

    /**
     * 单个插入
     * @param entity
     * @return
     */
    int insert(T entity);

    /**
     * 批量插入
     * @param entities
     * @return
     */
    int batchInsert(Collection<T> entities);

    /**
     * 根据id删除
     * @param id
     * @return
     */
    int deleteById(ID id);

    /**
     * 根据id批量删除
     * @param ids
     * @return
     */
    int deleteByIds(Collection<ID> ids);

    /**
     * 根据条件删除
     * @param condition
     * @return
     */
    int delete(Condition<T> condition);

    /**
     * 根据实体更新
     * @param entity
     * @return
     */
    int update(T entity);

    /**
     * 根据条件更新
     * @param condition
     * @return
     */
    int update(Condition<T> condition);

    /**
     * 通过id查询
     * @param id
     * @return
     */
    T selectById(ID id);

    /**
     * 通过id批量查询
     * @param ids
     * @return
     */
    List<T> selectByIds(Collection<ID> ids);

    /**
     * 通过条件查询一条
     * @param condition
     * @return
     */
    T selectOne(Condition<T> condition);

    /**
     * 通过条件查询列表
     * @param condition
     * @return
     */
    List<T> selectList(Condition<T> condition);

    /**
     * 通过条件查询map列表
     * @param condition
     * @return
     */
    List<Map<String, Object>> selectMaps(Condition<T> condition);

    /**
     * 通过sql更新
     * @param sql
     * @param args
     * @return
     */
    int updateBySql(String sql, Object...args);

    /**
     * 通过sql查询一条
     * @param sql
     * @param args
     * @return
     */
    T selectOneBySql(String sql, Object...args);

    /**
     * 通过sql查询列表
     * @param sql
     * @param args
     * @return
     */
    List<T> selectListBySql(String sql, Object...args);

    /**
     * 通过sql查询map列表
     * @param sql
     * @param args
     * @return
     */
    List<Map<String, Object>> selectMapsBySql(String sql, Object...args);

}
