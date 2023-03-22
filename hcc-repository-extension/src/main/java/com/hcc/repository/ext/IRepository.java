package com.hcc.repository.ext;

import com.hcc.repository.core.mapper.BaseMapper;

import java.io.Serializable;

/**
 * IRepository
 *
 * @author hushengjun
 * @date 2023/3/3
 */
public interface IRepository<T, ID extends Serializable> {

    BaseMapper<T, ID> getBaseMapper();

}
