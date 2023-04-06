package com.hcc.repository.test;

import com.github.jsonzou.jmockdata.JMockData;
import com.github.jsonzou.jmockdata.TypeReference;
import org.junit.Test;

import java.util.ArrayList;
import java.util.List;

/**
 * InsertTest
 *
 * @author hushengjun
 * @date 2023/3/22
 */
public class InsertTest extends BaseTest {

    @Test
    public void insert1() {
        for (int i = 0; i < 10; i++) {
            TableTestPo mock = JMockData.mock(TableTestPo.class);
            mapper.insert(mock);
        }
    }

    @Test
    public void batchInsert() {
        List<TableTestPo> list = new ArrayList<>();
        for (int i = 0; i < 100; i++) {
            list.add(JMockData.mock(TableTestPo.class));
        }

        mapper.batchInsert(list);
    }

}
