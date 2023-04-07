package com.hcc.repository.test.insert;

import com.github.jsonzou.jmockdata.JMockData;
import com.hcc.repository.test.BaseTest;
import com.hcc.repository.test.domain.po.ProductPo;
import org.junit.Test;

/**
 * InsertTest
 *
 * @author hushengjun
 * @date 2023/4/7
 */
public class InsertTest extends BaseTest {

    @Test
    public void insertTest() {
        for (int i = 0; i < 100; i++) {
            ProductPo productPo = JMockData.mock(ProductPo.class);
            productService.save(productPo);
        }
    }

}
