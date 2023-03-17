package com.hcc.repository.core.mapper;

import com.hcc.repository.core.condition.ICondition;

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
    int delete(ICondition<T> condition);

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
    int update(ICondition<T> condition);

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
    T selectOne(ICondition<T> condition);

    /**
     * 通过条件查询列表
     * @param condition
     * @return
     */
    List<T> selectList(ICondition<T> condition);

    /**
     * 查询id列表
     * @param condition
     * @return
     */
    List<ID> selectIds(ICondition<T> condition);

    /**
     * 通过条件查询总数
     * @param condition
     * @return
     */
    Long selectCount(ICondition<T> condition);

    /**
     * 通过条件查询map列表
     * @param condition
     * @return
     */
    List<Map<String, Object>> selectMaps(ICondition<T> condition);

}
