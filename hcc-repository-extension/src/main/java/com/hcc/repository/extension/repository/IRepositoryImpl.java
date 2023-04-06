package com.hcc.repository.extension.repository;

import com.hcc.repository.core.conditions.ICondition;
import com.hcc.repository.core.mapper.BaseMapper;
import com.hcc.repository.core.metadata.TableColumnInfo;
import com.hcc.repository.core.metadata.TableInfoHelper;
import com.hcc.repository.core.page.IPage;
import com.hcc.repository.core.utils.ReflectUtils;

import java.io.Serializable;
import java.util.Collection;

/**
 * IRepositoryImpl
 *
 * @author hushengjun
 * @date 2023/3/3
 */
public class IRepositoryImpl<M extends BaseMapper<T, ID>, T, ID extends Serializable> implements IRepository<T, ID> {

    private M mapper;

    public IRepositoryImpl(M mapper) {
        this.mapper = mapper;
    }

    @Override
    public M getBaseMapper() {
        return mapper;
    }

    @Override
    public boolean batchSave(Collection<T> entities) {
        entities.forEach(this::save);
        return true;
    }

    @Override
    public boolean saveOrUpdate(T entity) {
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

    @Override
    public IPage<T> page(ICondition<T> condition, IPage<T> pageParam) {
        return null;
    }

}
