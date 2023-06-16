package com.hcc.repository.extension.repository;

import com.hcc.repository.core.jdbc.JdbcOperations;
import com.hcc.repository.core.mapper.BaseMapper;
import com.hcc.repository.core.utils.Assert;
import org.springframework.beans.factory.annotation.Autowired;

import java.io.Serializable;

/**
 * 基础Repository接口实现类
 *
 * @author hushengjun
 * @date 2023/3/3
 */
public class IRepositoryImpl<M extends BaseMapper<T, ID>, T, ID extends Serializable> implements IRepository<T, ID> {

    @Autowired(required = false)
    protected M mapper;

    @Autowired(required = false)
    protected JdbcOperations jdbcOperations;

    protected IRepositoryImpl() {}

    protected IRepositoryImpl(M mapper, JdbcOperations jdbcOperations) {
        this.mapper = mapper;
        this.jdbcOperations = jdbcOperations;
    }

    @Override
    public M getBaseMapper() {
        Assert.isNotNull(mapper, "未成功注入该mapper，请通过带参构造方法注入");
        return mapper;
    }

}
