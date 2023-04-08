package com.hcc.repository.test.service.impl;

import com.hcc.repository.extension.repository.IRepository;
import com.hcc.repository.extension.repository.IRepositoryImpl;
import com.hcc.repository.test.domain.po.ProductPo;
import com.hcc.repository.test.dao.mapper.ProductMapper;
import com.hcc.repository.test.service.ProductService;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

/**
 * ProductServiceImpl
 *
 * @author hushengjun
 * @date 2023/4/6
 */
@Service
@Transactional(rollbackFor = Exception.class)
public class ProductServiceImpl extends IRepositoryImpl<ProductMapper, ProductPo, Long> implements ProductService {

}
