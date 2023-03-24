package com.hcc.repository.core;

import com.github.jsonzou.jmockdata.JMockData;
import com.hcc.repository.core.conditions.update.LambdaUpdateCondition;
import org.junit.Test;

import java.util.Arrays;

/**
 * DeleteTest
 *
 * @author hushengjun
 * @date 2023/3/22
 */
public class UpdateTest extends BaseTest {

    @Test
    public void updateById() {
        TableTestPo mock = JMockData.mock(TableTestPo.class);
        mock.setId(1L);

        mapper.updateById(mock);
    }

    @Test
    public void update() {
        LambdaUpdateCondition<TableTestPo> condition = new LambdaUpdateCondition<>();
        condition.set(TableTestPo::getAge, 1)
                .set(TableTestPo::getName, "hsj")
                .in(TableTestPo::getId, Arrays.asList(1L, 2L));
        mapper.update(condition);
    }

}
