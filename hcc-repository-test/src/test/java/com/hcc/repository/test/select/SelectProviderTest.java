package com.hcc.repository.test.select;

import com.hcc.repository.test.BaseTest;
import com.hcc.repository.test.domain.po.ProductPo;
import org.junit.Test;

/**
 * SelectProviderTest
 *
 * @author hushengjun
 * @date 2023/7/25
 */
public class SelectProviderTest extends BaseTest {

    @Test
    public void selectProvider1Test() {
        ProductPo productPo = productMapper.selectProvider1(375700039445909504L);
    }

}
