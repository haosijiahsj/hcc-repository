package com.hcc.repository.test.select;

import com.hcc.repository.core.conditions.query.LambdaQueryCondition;
import com.hcc.repository.core.page.DefaultPage;
import com.hcc.repository.core.page.IPage;
import com.hcc.repository.test.BaseTest;
import com.hcc.repository.test.domain.ProductQueryParam;
import com.hcc.repository.test.domain.po.ProductPo;
import org.junit.Test;

import java.util.Arrays;
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

    @Test
    public void pageTest() {
        IPage<ProductPo> page = productService.lambdaQuery().page(new DefaultPage<>());
        System.out.println(page);
    }

    @Test
    public void queryAnnotationTest() {
//        ProductPo query = productMapper.query(1);
//        IPage<ProductPo> page = productMapper.queryPage(1, new DefaultPage());
        LambdaQueryCondition<ProductPo> condition = new LambdaQueryCondition<>();
//        condition.eq(ProductPo::getProductStatus, 1);
//        IPage<ProductPo> page1 = productMapper.queryPageCondition(condition, new DefaultPage());
//        productMapper.updateSql("hhssjj", condition);
        ProductQueryParam param = new ProductQueryParam();
        param.setName("hsj");
        param.setId(1L);
        param.setIds(Arrays.asList(1L, 2L, 3L));
        List<ProductPo> productPos = productMapper.selectProducts(param);
        System.out.println(productPos);
    }

}
