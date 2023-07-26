package com.hcc.repository.test.update;

import com.hcc.repository.test.BaseTest;
import com.hcc.repository.test.domain.enums.ProductStatusEnum;
import com.hcc.repository.test.domain.po.ProductPo;
import org.junit.Test;

import java.math.BigDecimal;

/**
 * UpdateTest
 *
 * @author hushengjun
 * @date 2023/4/8
 */
public class UpdateTest extends BaseTest {

    @Test
    public void updateById() {
//        productService.lambdaUpdate().set(ProductPo::getName, "hsj").update();
        ProductPo productPo = new ProductPo();
        productPo.setId(395898821219323904L);
        productPo.setName("hsj-test");
        productPo.setPrice(new BigDecimal("100.01"));
        productPo.setProductStatus(ProductStatusEnum.SHELF);
        productService.updateById(productPo);
    }

    @Test
    public void updateEntity() {
        ProductPo productPo = new ProductPo();
        productPo.setId(1L);
        productPo.setName("hsj");
        productPo.setPrice(new BigDecimal("100.01"));
        productService.lambdaUpdate().set(ProductPo::getName, "hsj").eq(ProductPo::getId, 1L).update(productPo);
    }

    @Test
    public void updateVersion() {
        ProductPo productPo = productService.getById(1L);
        productPo.setName("hcc_1");
        productService.updateById(productPo);
    }

    @Test
    public void updateByAnnotation() {
        ProductPo productPo = productService.getById(375700039445909504L);
        productPo.setName("hcc_1");
        productMapper.updateDynamic(productPo);
    }

}
