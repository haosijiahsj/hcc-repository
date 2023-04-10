package com.hcc.repository.extension.repository;

import com.hcc.repository.core.jdbc.JdbcTemplateProxy;
import com.hcc.repository.core.mapper.BaseMapper;
import org.springframework.beans.factory.annotation.Autowired;

import java.io.Serializable;

/**
 * 基础Repository接口实现类
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

}
