package com.hcc.repository.extension.repository;

import com.hcc.repository.core.conditions.ICondition;
import com.hcc.repository.core.mapper.BaseMapper;
import com.hcc.repository.core.metadata.TableColumnInfo;
import com.hcc.repository.core.metadata.TableInfoHelper;
import com.hcc.repository.core.page.IPage;
import com.hcc.repository.core.utils.CollUtils;
import com.hcc.repository.core.utils.ReflectUtils;
import com.hcc.repository.extension.conditions.ChainConditions;
import com.hcc.repository.extension.conditions.OriginalSqlChainCondition;
import com.hcc.repository.extension.conditions.query.DefaultQueryChainCondition;
import com.hcc.repository.extension.conditions.query.LambdaQueryChainCondition;
import com.hcc.repository.extension.conditions.update.DefaultUpdateChainCondition;
import com.hcc.repository.extension.conditions.update.LambdaUpdateChainCondition;

import java.io.Serializable;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 * IRepository
 *
 * @author hushengjun
 * @date 2023/3/3
 */
public interface IRepository<T, ID extends Serializable> {

    /**
     * 获取BaseMapper
     * @return
     */
    BaseMapper<T, ID> getBaseMapper();

    /**
     * 默认的链式查询
     * @return
     */
    default DefaultQueryChainCondition<T, ID> defaultQuery() {
        return ChainConditions.defaultQuery(getBaseMapper());
    }

    /**
     * lambda的链式查询
     * @return
     */
    default LambdaQueryChainCondition<T, ID> lambdaQuery() {
        return ChainConditions.lambdaQuery(getBaseMapper());
    }

    /**
     * 默认的链式更新
     * @return
     */
    default DefaultUpdateChainCondition<T, ID> defaultUpdate() {
        return ChainConditions.defaultUpdate(getBaseMapper());
    }

    /**
     * lambda的链式更新
     * @return
     */
    default LambdaUpdateChainCondition<T, ID> lambdaUpdate() {
        return ChainConditions.lambdaUpdate(getBaseMapper());
    }

    /**
     * 原生的sql操作
     * @return
     */
    default OriginalSqlChainCondition<T, ID> originalSql() {
        return ChainConditions.originalSql(getBaseMapper());
    }

    /**
     * 保存实体数据
     * @param entity
     * @return
     */
    default boolean save(T entity) {
        return getBaseMapper().insert(entity) >= 1;
    }

    /**
     * 批量保存实体数据
     * @param entities
     * @return
     */
    boolean batchSave(Collection<T> entities);

    /**
     * 通过id更新实体
     * @param entity
     * @return
     */
    default boolean updateById(T entity) {
        return getBaseMapper().updateById(entity) >= 1;
    }

    /**
     * 通过条件更新
     * @param condition
     * @return
     */
    default boolean update(ICondition<T> condition) {
        return getBaseMapper().update(condition) >= 1;
    }

    /**
     * 保存或更新，通过是否传入id判断，若存在id，则查询是否存在判断
     * @param entity
     * @return
     */
    default boolean saveOrUpdate(T entity) {
        TableColumnInfo idColumnInfo = TableInfoHelper.getIdColumnInfo(entity.getClass());
        if (idColumnInfo == null) {
            return false;
        }

        Object idValue = ReflectUtils.getValue(entity, idColumnInfo.getField());
        if (idValue == null) {
            return save(entity);
        }

        // 通过构建condition查询，避免强转
        T existEntity = defaultQuery()
                .select(idColumnInfo.getColumnName())
                .eq(idColumnInfo.getColumnName(), idValue)
                .one();
        if (existEntity == null) {
            return save(entity);
        }

        return updateById(entity);
    }

    /**
     * 通过id删除数据
     * @param id
     * @return
     */
    default boolean removeById(ID id) {
        return getBaseMapper().deleteById(id) >= 1;
    }

    /**
     * 通过id列表删除数据
     * @param ids
     * @return
     */
    default boolean removeByIds(Collection<ID> ids) {
        return getBaseMapper().deleteByIds(ids) >= 1;
    }

    /**
     * 通过条件删除数据
     * @param condition
     * @return
     */
    default boolean remove(ICondition<T> condition) {
        return getBaseMapper().delete(condition) >= 1;
    }

    /**
     * 通过id获取一条记录
     * @param id
     * @return
     */
    default T getById(ID id) {
        return getBaseMapper().selectById(id);
    }

    /**
     * 通过id列表获取列表
     * @param ids
     * @return
     */
    default List<T> listByIds(Collection<ID> ids) {
        return getBaseMapper().selectByIds(ids);
    }

    /**
     * 通过条件获取一条数据，当数据多条时抛出异常
     * @param condition
     * @return
     */
    default T getOne(ICondition<T> condition) {
        return getBaseMapper().selectOne(condition);
    }

    /**
     * 通过条件获取一条数据，当数据多条时返回第一条
     * @param condition
     * @return
     */
    default T getOneSafe(ICondition<T> condition) {
        List<T> results = list(condition);
        if (CollUtils.isEmpty(results)) {
            return null;
        }

        return results.get(0);
    }

    /**
     * 跳过条件查询列表
     * @param condition
     * @return
     */
    default List<T> list(ICondition<T> condition) {
        return getBaseMapper().selectList(condition);
    }

    /**
     * 通过条件查询id列表
     * @param condition
     * @return
     */
    default List<ID> listIds(ICondition<T> condition) {
        return getBaseMapper().selectIds(condition);
    }

    /**
     * 通过条件查询对象列表
     * @param condition
     * @return
     */
    default List<Object> listObjects(ICondition<T> condition) {
        return getBaseMapper().selectObjects(condition);
    }

    /**
     * 通过条件查询对象列表，自定义转换方法
     * @param condition
     * @return
     */
    default <R> List<R> listObjects(ICondition<T> condition, Function<Object, R> mapper) {
        return listObjects(condition).stream()
                .filter(Objects::nonNull)
                .map(mapper)
                .collect(Collectors.toList());
    }

    /**
     * 获取一个对象
     * @param condition
     * @param mapper
     * @param <R>
     * @return
     */
    default <R> R getObject(ICondition<T> condition, Function<Object, R> mapper) {
        return listObjects(condition, mapper).stream()
                .findFirst()
                .orElse(null);
    }

    /**
     * 通过条件查询总数
     * @param condition
     * @return
     */
    default Long count(ICondition<T> condition) {
        return getBaseMapper().selectCount(condition);
    }

    /**
     * 通过条件查询map列表
     * @param condition
     * @return
     */
    default List<Map<String, Object>> listMaps(ICondition<T> condition) {
        return getBaseMapper().selectMaps(condition);
    }

    /**
     * 通过map查询列表
     * @param paramMap
     * @return
     */
    default List<T> listByMap(Map<String, Object> paramMap) {
        return getBaseMapper().selectListByMap(paramMap);
    }

    /**
     * 通过条件分页
     * @param condition
     * @param pageParam
     * @return
     */
    IPage<T> page(ICondition<T> condition, IPage<T> pageParam);

}
