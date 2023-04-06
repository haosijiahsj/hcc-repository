package com.hcc.repository.starter.autoconfigure;

import lombok.Data;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Configuration;

import java.util.List;

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

    private boolean enableBanner = true;
    private List<String> entityPackages;
    private List<String> basePackages;

}
