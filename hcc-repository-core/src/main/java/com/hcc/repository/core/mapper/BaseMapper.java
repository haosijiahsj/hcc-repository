package com.hcc.repository.core.mapper;

import com.hcc.repository.core.conditions.ICondition;
import com.hcc.repository.core.exceptions.RepositoryException;
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
     * 批量插入
     * @param entities
     * @return
     */
    default int[] batchInsert(Collection<T> entities) {
        Integer[] rs = Optional.ofNullable(entities)
                .orElse(Collections.emptyList())
                .stream()
                .map(this::insert)
                .toArray(Integer[]::new);
        int[] r = new int[rs.length];
        for (int i = 0; i < rs.length; i++) {
            r[i] = rs[i] == null ? 0 : rs[i];
        }

        return r;
    }

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
     * 根据实体更新
     * @param entity
     * @return
     */
    int updateById(T entity);

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
        if (results.isEmpty()) {
            return null;
        }
        if (results.size() > 1) {
            throw new RepositoryException(String.format("预期一条数据，实际%s条数据", results.size()));
        }

        return results.get(0);
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
                        throw new RepositoryException("存在多列数据");
                    }

                    return m.values().stream().findFirst().orElse(null);
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
     * 分页查询
     * @param condition
     * @param pageParam
     * @return
     */
    @Deprecated
    IPage<T> selectPage(ICondition<T> condition, IPage<T> pageParam);

}
