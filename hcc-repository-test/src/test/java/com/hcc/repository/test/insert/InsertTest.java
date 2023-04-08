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
        for (int i = 0; i < 10; i++) {
            ProductPo productPo = JMockData.mock(ProductPo.class);
//            productPo.setDeleted(0);
            productService.save(productPo);
            System.out.println("插入的id: " + productPo.getId());
        }
    }

}
