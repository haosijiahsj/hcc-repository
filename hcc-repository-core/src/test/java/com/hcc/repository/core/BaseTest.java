package com.hcc.repository.core;

import com.alibaba.druid.pool.DruidDataSource;
import com.hcc.repository.annotation.Id;
import com.hcc.repository.annotation.Table;
import com.hcc.repository.core.mapper.BaseMapper;
import com.hcc.repository.core.proxy.InjectMapperProxyFactory;
import lombok.Data;
import org.junit.Before;

import java.time.LocalDateTime;

/**
 * BaseTest
 *
 * @author hushengjun
 * @date 2023/3/22
 */
public class BaseTest {

    private static final String DRIVER_CLASS_NAME = "com.mysql.jdbc.Driver";

    protected TestMapper mapper;

    @Before
    public void init() {
        DruidDataSource druidDataSource = new DruidDataSource();
        druidDataSource.setDriverClassName(DRIVER_CLASS_NAME);
//        String dbName = "oa_workflow";
//        druidDataSource.setUrl("jdbc:mysql://cd-cdb-83zxqgz2.sql.tencentcdb.com:62363/"+ dbName +"?characterEncoding=utf-8");
//        druidDataSource.setUsername("duxq_read");
//        druidDataSource.setPassword("IKOjVXJ4");

        mapper = InjectMapperProxyFactory.create(TestMapper.class, druidDataSource);
    }

    public interface TestMapper extends BaseMapper<TableTestPo, Long> {}

    @Data
    @Table("table_test")
    public static class TableTestPo {
        @Id
        private Long id;
        private String name;
        private Integer sex;
        private Integer age;
        private LocalDateTime createTime;
        private LocalDateTime updateTime;
    }

}
