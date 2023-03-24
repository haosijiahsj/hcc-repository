package com.hcc.repository.core;

import com.alibaba.druid.pool.DruidDataSource;
import com.hcc.repository.annotation.Column;
import com.hcc.repository.annotation.Id;
import com.hcc.repository.annotation.Table;
import com.hcc.repository.core.converter.CustomerConverter;
import com.hcc.repository.core.enums.SexEnum;
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

    private static final String DRIVER_CLASS_NAME = "com.mysql.cj.jdbc.Driver";

    protected TestMapper mapper;

    @Before
    public void init() {
        DruidDataSource druidDataSource = new DruidDataSource();
        druidDataSource.setDriverClassName(DRIVER_CLASS_NAME);
        druidDataSource.setUrl("jdbc:mysql://localhost:3306/hcc_repository?characterEncoding=utf-8&serverTimezone=Asia/Shanghai");
        druidDataSource.setUsername("root");
        druidDataSource.setPassword("123456");

        mapper = InjectMapperProxyFactory.create(TestMapper.class, druidDataSource);
    }

    public interface TestMapper extends BaseMapper<TableTestPo, Long> {}

    @Data
    @Table("table_test")
    public static class TableTestPo {
        @Id
        private Long id;
        @Column("name_")
        private String name;
        @Column(converter = CustomerConverter.class)
        private SexEnum sex;
        private Integer age;
        private LocalDateTime createTime;
        private LocalDateTime updateTime;
    }

}
