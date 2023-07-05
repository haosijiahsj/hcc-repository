package com.hcc.repository.starter.autoconfigure;

import lombok.Data;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Configuration;

import java.util.List;
import java.util.Map;

/**
 * RepositoryProperties
 *
 * @author hushengjun
 * @date 2023/4/5
 */
@Data
@Configuration
@ConfigurationProperties(prefix = "hcc.repository")
public class RepositoryProperties {

    /**
     * 是否开启banner
     */
    private boolean enableBanner = true;
    /**
     * 是否打印sql日志
     */
    private boolean printSqlLog = false;
    /**
     * 批量插入限制数量
     */
    private int batchInsertLimitSize = 500;
    /**
     * 实体包路径
     */
    private List<String> entityPackages;
    /**
     * mapper包路径
     */
    private List<String> basePackages;
    /**
     * 其它配置
     */
    private Map<String, Object> properties;

}
