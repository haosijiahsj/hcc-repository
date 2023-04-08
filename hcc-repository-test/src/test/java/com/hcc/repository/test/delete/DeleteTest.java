package com.hcc.repository.test.delete;

import com.hcc.repository.test.BaseTest;
import com.hcc.repository.test.domain.po.ProductPo;
import org.junit.Test;

/**
 * DeleteTest
 *
 * @author hushengjun
 * @date 2023/4/8
 */
public class DeleteTest extends BaseTest {

    @Test
    public void deleteById() {
        productService.lambdaUpdate().eq(ProductPo::getId, 3).remove();
        productService.removeById(4L);
    }

}
