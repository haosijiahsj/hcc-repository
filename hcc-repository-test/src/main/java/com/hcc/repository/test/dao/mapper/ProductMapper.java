package com.hcc.repository.test.dao.mapper;

import com.hcc.repository.core.mapper.BaseMapper;
import com.hcc.repository.core.spring.DS;
import com.hcc.repository.test.domain.po.ProductPo;

/**
 * ProductMapper
 *
 * @author hushengjun
 * @date 2023/4/6
 */
//@DS("dataSource")
public interface ProductMapper extends BaseMapper<ProductPo, Long> {
}
