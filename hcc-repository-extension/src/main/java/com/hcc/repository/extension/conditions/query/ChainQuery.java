package com.hcc.repository.extension.conditions.query;

import com.hcc.repository.core.conditions.ICondition;
import com.hcc.repository.core.constants.ExecuteSqlTypeEnum;
import com.hcc.repository.core.page.IPage;
import com.hcc.repository.extension.conditions.ChainCondition;

import java.io.Serializable;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 * ChainQuery
 *
 * @author hushengjun
 * @date 2023/4/2
 */
public interface ChainQuery<T, ID extends Serializable> extends ChainCondition<T, ID> {

    /**
     * 查询列表
     * @return
     */
    default List<T> list() {
        return getBaseMapper().selectList(getCondition());
    }

    /**
     * 查询id列表
     * @return
     */
    default List<ID> listIds() {
        return getBaseMapper().selectIds(getCondition());
    }

    /**
     * 查询对象列表，Long、String==
     * @param mapper
     * @return
     * @param <R>
     */
    default <R> List<R> listObjects(Function<Object, R> mapper) {
        return getBaseMapper()
                .selectObjects(getCondition())
                .stream()
                .map(mapper)
                .collect(Collectors.toList());
    }

    /**
     * 查询map列表
     * @return
     */
    default List<Map<String, Object>> listMaps() {
        return getBaseMapper().selectMaps(getCondition());
    }

    /**
     * 查询一个实体
     * @return
     */
    default T one() {
        return getBaseMapper().selectOne(getCondition());
    }

    /**
     * 查询一个实体
     * @return
     */
    default Optional<T> oneOpt() {
        return Optional.ofNullable(this.one());
    }

    /**
     * 查询count
     * @return
     */
    default Long count() {
        ICondition<T> condition = getCondition();
        condition.setExecuteSqlType(ExecuteSqlTypeEnum.SELECT_COUNT);
        return getBaseMapper().selectCount(getCondition());
    }

    /**
     * 查询是否存在，注意请控制查询一条即可
     * @return
     */
    default boolean exist() {
        return list().size() >= 1;
    }

    /**
     * 分页查询
     * @param pageParam
     * @return
     */
    default IPage<T> page(IPage<T> pageParam) {
        return getBaseMapper().selectPage(getCondition(), pageParam);
    }

}
