package com.hcc.repository.test.select;

import com.hcc.repository.test.BaseTest;
import com.hcc.repository.test.domain.po.ProductPo;
import org.junit.Test;

import java.util.List;

/**
 * SelectTest
 *
 * @author hushengjun
 * @date 2023/4/7
 */
public class SelectTest extends BaseTest {

    @Test
    public void listTest() {
        List<ProductPo> list = productService.lambdaQuery().list();
        ProductPo list1 = productService.lambdaQuery().eq(ProductPo::getId, 1).one();
    }

    @Test
    public void complexListTest() {
        productService.lambdaQuery().or(c -> c.like(ProductPo::getName, "a")).or().eq(ProductPo::getId, 1).list();
        productService.originalSql().sql("select * from product limit 1").one();
    }

}
