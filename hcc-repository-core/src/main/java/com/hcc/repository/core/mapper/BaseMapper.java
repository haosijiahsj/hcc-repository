package com.hcc.repository.core.mapper;

import com.hcc.repository.core.conditions.ICondition;
import com.hcc.repository.core.exceptions.IncorrectColumnCountException;
import com.hcc.repository.core.exceptions.TooManyResultException;
import com.hcc.repository.core.page.IPage;

import java.io.Serializable;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;

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
     * 从Condition插入
     * @param condition
     * @return
     */
    int insertByCondition(ICondition<T> condition);

    /**
     * 批量插入
     * @param entities
     * @return
     */
    default int[] batchInsert(Collection<T> entities) {
        return Optional.ofNullable(entities)
                .orElse(Collections.emptyList())
                .stream()
                .mapToInt(this::insert)
                .toArray();
    }

    /**
     * 拼接式批量插入
     * @param entities
     * @return
     */
    int batchInsertSplice(Collection<T> entities);

    /**
     * 根据id删除
     * @param id
     * @return
     */
    default int deleteById(ID id) {
        return deleteByIds(Collections.singletonList(id));
    }

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
     * 根据实体更新，必须有id字段
     * @param entity
     * @return
     */
    default int updateById(T entity) {
        return updateById(entity, false);
    }

    /**
     * 通过id更新，nullSet为true则会更新为null的字段
     * @param entity
     * @param nullSet
     * @return
     */
    int updateById(T entity, boolean nullSet);

    /**
     * 通过条件更新实体
     * @param entity
     * @param condition
     * @return
     */
    default int updateEntity(T entity, ICondition<T> condition) {
        return updateEntity(entity, condition, false);
    }

    /**
     * 通过条件更新实体，nullSet为true则会更新为null的字段
     * @param entity
     * @param condition
     * @param nullSet
     * @return
     */
    int updateEntity(T entity, ICondition<T> condition, boolean nullSet);

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
    default T selectById(ID id) {
        return Optional.ofNullable(selectByIds(Collections.singletonList(id)))
                .orElseGet(Collections::emptyList)
                .stream()
                .findFirst()
                .orElse(null);
    }

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
    default T selectOne(ICondition<T> condition) {
        List<T> results = selectList(condition);
        if (results.size() > 1) {
            throw new TooManyResultException(1, results.size());
        }

        return results.stream()
                .findFirst()
                .orElse(null);
    }

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
     * 查询对象列表，取第一列数据
     * @param condition
     * @return
     */
    default List<Object> selectObjects(ICondition<T> condition) {
        return selectMaps(condition).stream()
                .map(m -> {
                    if (m.size() > 1) {
                        throw new IncorrectColumnCountException(1, m.size());
                    }
                    return m.values().stream()
                            .findFirst()
                            .orElse(null);
                })
                .collect(Collectors.toList());
    }

    /**
     * 通过条件查询map列表
     * @param condition
     * @return
     */
    List<Map<String, Object>> selectMaps(ICondition<T> condition);

    /**
     * 通过map条件查询列表
     * @param paramMap
     * @return
     */
    List<T> selectListByMap(Map<String, Object> paramMap);

    /**
     * 通过map查询一条
     * @param paramMap
     * @return
     */
    default T selectOneByMap(Map<String, Object> paramMap) {
        List<T> results = selectListByMap(paramMap);
        if (results.size() > 1) {
            throw new TooManyResultException(1, results.size());
        }

        return results.stream()
                .findFirst()
                .orElse(null);
    }

    /**
     * 分页查询
     * @param condition
     * @param pageParam
     * @return
     */
    IPage<T> selectPage(ICondition<T> condition, IPage<T> pageParam);

    /**
     * 分页查询map列表
     * @param condition
     * @param pageParam
     * @return
     */
    IPage<Map<String, Object>> selectMapsPage(ICondition<T> condition, IPage<T> pageParam);

}
