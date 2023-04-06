package com.hcc.repository.test;

import org.junit.Test;

import java.util.List;

/**
 * RepositorySelect
 *
 * @author hushengjun
 * @date 2023/4/4
 */
public class RepositorySelectTest extends BaseTest {

    @Test
    public void selectById() {
        TableTestPo tableTestPo = testRepository.getById(1L);
        System.out.println(tableTestPo);
    }

    @Test
    public void selectLambdaQueryOne() {
        TableTestPo tableTestPo = testRepository.lambdaQuery().eq(TableTestPo::getId, 1L).one();
        System.out.println(tableTestPo);
    }

    @Test
    public void selectLambdaQueryList() {
        List<TableTestPo> list = testRepository.lambdaQuery().eq(TableTestPo::getSex, 1).list();
        System.out.println(list);
    }

    @Test
    public void selectLambdaQueryListIds() {
        List<Long> list = testRepository.lambdaQuery().eq(TableTestPo::getSex, 1).listIds();
        System.out.println(list);
    }

    @Test
    public void selectLambdaQueryListObjects() {
        List<String> list = testRepository.lambdaQuery()
                .select(TableTestPo::getName)
                .eq(TableTestPo::getSex, 1)
                .listObjects(Object::toString);
        System.out.println(list);
    }

    @Test
    public void selectLambdaQueryCount() {
        Long count = testRepository.lambdaQuery()
                .select(TableTestPo::getName)
                .eq(TableTestPo::getSex, 1)
                .count();
        System.out.println(count);
    }

}
