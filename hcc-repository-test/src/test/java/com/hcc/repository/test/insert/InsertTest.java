package com.hcc.repository.test.insert;

import com.github.jsonzou.jmockdata.JMockData;
import com.hcc.repository.core.conditions.insert.DefaultInsertCondition;
import com.hcc.repository.core.conditions.insert.LambdaInsertCondition;
import com.hcc.repository.test.BaseTest;
import com.hcc.repository.test.domain.enums.ProductStatusEnum;
import com.hcc.repository.test.domain.po.ProductPo;
import org.junit.Test;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;

/**
 * InsertTest
 *
 * @author hushengjun
 * @date 2023/4/7
 */
public class InsertTest extends BaseTest {

    @Test
    public void insertTest() {
        for (int i = 0; i < 5; i++) {
            ProductPo productPo = JMockData.mock(ProductPo.class);
            productPo.setName("hsj");
            productPo.setCreateTime(null);
            productService.save(productPo);
            System.out.println("插入的id: " + productPo.getId());
        }
    }

    @Test
    public void insertTest1() {
        for (int i = 0; i < 10; i++) {
            ProductPo productPo = JMockData.mock(ProductPo.class);
            LambdaInsertCondition<ProductPo> condition = new LambdaInsertCondition<ProductPo>()
                    .value(ProductPo::getName, "hsj")
                    .value(ProductPo::getPrice, new BigDecimal("1.1"))
                    .value(ProductPo::getProductStatus, ProductStatusEnum.SHELF.getValue());
            int row = productService.getBaseMapper().insertByCondition(condition);
            System.out.println("插入的id: " + productPo.getId());
        }
    }

    @Test
    public void insertTest2() {
        List<ProductPo> productPos = new ArrayList<>();
        for (int i = 0; i < 10; i++) {
            ProductPo productPo = JMockData.mock(ProductPo.class);
            productPos.add(productPo);
        }
        productService.batchSaveSplice(productPos);
    }

    @Test
    public void insertTest3() {
        List<ProductPo> productPos = new ArrayList<>();
        for (int i = 0; i < 100; i++) {
            ProductPo productPo = JMockData.mock(ProductPo.class);
            productPo.setCreateTime(null);
            productPo.setUpdateTime(null);
            productPos.add(productPo);
        }
        productService.batchSave(productPos);
    }

}
