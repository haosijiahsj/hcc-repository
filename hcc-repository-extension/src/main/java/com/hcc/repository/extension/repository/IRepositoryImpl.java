package com.hcc.repository.extension.repository;

import com.hcc.repository.core.conditions.ICondition;
import com.hcc.repository.core.jdbc.JdbcTemplateProxy;
import com.hcc.repository.core.mapper.BaseMapper;
import com.hcc.repository.core.page.IPage;
import org.springframework.beans.factory.annotation.Autowired;

import java.io.Serializable;
import java.util.Collection;

/**
 * IRepositoryImpl
 *
 * @author hushengjun
 * @date 2023/3/3
 */
public class IRepositoryImpl<M extends BaseMapper<T, ID>, T, ID extends Serializable> implements IRepository<T, ID> {

    @Autowired
    protected M mapper;

    @Autowired
    protected JdbcTemplateProxy jdbcTemplateProxy;

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
    public IPage<T> page(ICondition<T> condition, IPage<T> pageParam) {
        return null;
    }

}
