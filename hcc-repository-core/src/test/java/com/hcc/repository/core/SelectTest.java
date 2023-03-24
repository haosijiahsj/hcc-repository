package com.hcc.repository.core;

import com.hcc.repository.core.conditions.Conditions;
import com.hcc.repository.core.conditions.query.LambdaQueryCondition;
import org.junit.Test;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

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

    @Test
    public void selectByIds() {
        List<TableTestPo> tableTestPos = mapper.selectByIds(Arrays.asList(3L, 4L));
        System.out.println(tableTestPos);
    }

    @Test
    public void selectCount() {
        LambdaQueryCondition<TableTestPo> condition = Conditions.<TableTestPo>lambdaQuery()
                .likeRight(TableTestPo::getName, "C");
        Long count = mapper.selectCount(condition);
        System.out.println(count);
    }

    @Test
    public void selectIds() {
        LambdaQueryCondition<TableTestPo> condition = Conditions.<TableTestPo>lambdaQuery()
                .likeRight(TableTestPo::getName, "C");
        List<Long> ids = mapper.selectIds(condition);
        System.out.println(ids);
    }

    @Test
    public void selectList() {
        LambdaQueryCondition<TableTestPo> condition = Conditions.<TableTestPo>lambdaQuery()
                .select(TableTestPo::getId, TableTestPo::getName, TableTestPo::getAge)
                .eq(TableTestPo::getSex, 1)
                .orderByDesc(TableTestPo::getId);
        List<TableTestPo> tableTestPos = mapper.selectList(condition);
        System.out.println(tableTestPos);
    }

    @Test
    public void selectOne() {
        LambdaQueryCondition<TableTestPo> condition = Conditions.<TableTestPo>lambdaQuery()
                .select(TableTestPo::getId, TableTestPo::getName, TableTestPo::getAge)
                .eq(TableTestPo::getSex, 1)
                .last("LIMIT 1")
                .orderByDesc(TableTestPo::getId);
        TableTestPo tableTestPo = mapper.selectOne(condition);
        System.out.println(tableTestPo);
    }

    @Test
    public void selectMaps() {
        LambdaQueryCondition<TableTestPo> condition = Conditions.<TableTestPo>lambdaQuery()
                .eq(TableTestPo::getSex, 1)
                .orderByDesc(TableTestPo::getId);
        List<Map<String, Object>> maps = mapper.selectMaps(condition);
        System.out.println(maps);
    }

}
