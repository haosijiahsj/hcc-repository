package com.hcc.repository.test;

import org.junit.Test;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * JdbcTemplateProxyTest
 *
 * @author hushengjun
 * @date 2023/3/26
 */
public class JdbcTemplateProxyTest extends BaseTest {

    @Test
    public void updateTest() {
        Map<String, Object> paramMap = new HashMap<>();
        paramMap.put("id", 2L);
        String sql = "select * from table_test where id=:id";
        List<Map<String, Object>> maps = jdbcTemplateProxy.namedQueryForList(sql, paramMap);
        System.out.println(maps);
    }

}
