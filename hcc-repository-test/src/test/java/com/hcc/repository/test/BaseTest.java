package com.hcc.repository.test;

import com.hcc.repository.test.dao.mapper.ProductMapper;
import com.hcc.repository.test.service.ProductService;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.junit4.SpringRunner;

/**
 * BaseTest
 *
 * @author hushengjun
 * @date 2023/4/7
 */
@RunWith(value = SpringRunner.class)
@SpringBootTest(classes = Application.class)
public class BaseTest {

    @Autowired
    protected ProductService productService;

    @Autowired
    protected ProductMapper productMapper;

}
