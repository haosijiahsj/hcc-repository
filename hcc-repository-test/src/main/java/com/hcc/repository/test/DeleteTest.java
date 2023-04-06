package com.hcc.repository.test;

import com.hcc.repository.core.conditions.Conditions;
import com.hcc.repository.core.conditions.query.LambdaQueryCondition;
import org.junit.Test;

import java.util.Arrays;

/**
 * DeleteTest
 *
 * @author hushengjun
 * @date 2023/3/22
 */
public class DeleteTest extends BaseTest {

    @Test
    public void deleteById() {
        mapper.deleteById(1L);
    }

    @Test
    public void deleteByIds() {
        mapper.deleteByIds(Arrays.asList(1L, 2L));
    }

    @Test
    public void delete() {
        LambdaQueryCondition<TableTestPo> condition = Conditions.<TableTestPo>lambdaQuery()
                .in(TableTestPo::getId, 2L);
        mapper.delete(condition);
    }

}
