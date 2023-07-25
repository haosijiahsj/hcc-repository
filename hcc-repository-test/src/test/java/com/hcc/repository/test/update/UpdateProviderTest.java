package com.hcc.repository.test.update;

import com.hcc.repository.test.BaseTest;
import org.junit.Test;

/**
 * UpdateProviderTest
 *
 * @author hushengjun
 * @date 2023/7/25
 */
public class UpdateProviderTest extends BaseTest {

    @Test
    public void updateProvider1Test() {
        int i = productMapper.updateProvider1(375700039445909504L);
    }

}
