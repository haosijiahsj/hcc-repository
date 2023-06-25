package com.hcc.repository.test.select;

import com.hcc.repository.core.conditions.query.DefaultQueryCondition;
import com.hcc.repository.core.conditions.query.LambdaQueryCondition;
import com.hcc.repository.core.page.DefaultPage;
import com.hcc.repository.core.page.IPage;
import com.hcc.repository.test.BaseTest;
import com.hcc.repository.test.domain.ProductQueryParam;
import com.hcc.repository.test.domain.po.ProductPo;
import org.junit.Test;

import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

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
        productService.lambdaQuery()
                .selectDistinct(ProductPo::getId)
                .or(c -> c.like(ProductPo::getName, "a"))
                .or()
                .eq(ProductPo::getId, 1)
                .list();
        productService.nativeSql().sql("select * from product limit 1").one();
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
//        List<ProductPo> productPos = productMapper.selectProducts(param);
//        System.out.println(productPos);

//        productMapper.selectProducts1(1L, Arrays.asList(1L, 2L, 3L), Arrays.asList("hsj"));

        Map<String, Object> paramMap = new HashMap<>();
        paramMap.put("id", 1L);
        paramMap.put("ids", Arrays.asList(1L, 2L, 3L));
//        paramMap.put("names", Arrays.asList("abc"));
        productMapper.selectProducts2(paramMap);
    }

    @Test
    public void queryAnnotationTest1() {
        List<Long> ids = productMapper.selectIds();
        System.out.println(ids);

        long id = productMapper.selectId();
        System.out.println(id);
    }

    @Test
    public void queryAnnotationTest2() {
        List<Long> ids = productMapper.selectIds();
        System.out.println(ids);

//        LambdaQueryCondition<ProductPo> condition = new LambdaQueryCondition<>();
//        condition.eq(ProductPo::getId, 1L);
        DefaultQueryCondition<ProductPo> condition = new DefaultQueryCondition<>();
        condition.eq("p.id", 1L);
        long id = productMapper.selectByAnnotationAndCondition(condition, "h");
        System.out.println(id);
    }

    @Test
    public void queryAnnotationTest3() {
        long id = productMapper.selectByAnnotationForArgIndex("h");
        System.out.println(id);
    }

}
