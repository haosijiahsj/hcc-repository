package com.hcc.repository.core;

import org.junit.Test;

/**
 * SelectTest
 *
 * @author hushengjun
 * @date 2023/3/22
 */
public class SelectTest extends BaseTest {

    @Test
    public void selectById() {
        TableTestPo tableTestPo = mapper.selectById(50L);
        System.out.println(tableTestPo);
    }

}
