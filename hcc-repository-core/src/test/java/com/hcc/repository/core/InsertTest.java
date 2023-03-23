package com.hcc.repository.core;

import com.github.jsonzou.jmockdata.JMockData;
import com.github.jsonzou.jmockdata.TypeReference;
import org.junit.Test;

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
        for (int i = 0; i < 100; i++) {
            TableTestPo mock = JMockData.mock(TableTestPo.class);
            mock.setSex(i % 2);
            mapper.insert(mock);
        }
    }

    @Test
    public void batchInsert() {
        List<TableTestPo> list = JMockData.mock(new TypeReference<List<TableTestPo>>() {});

        mapper.batchInsert(list);
    }

}
