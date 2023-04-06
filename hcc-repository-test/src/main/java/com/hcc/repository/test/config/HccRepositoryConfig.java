package com.hcc.repository.test.config;

import com.hcc.repository.core.spring.MapperScan;
import org.springframework.context.annotation.Configuration;

/**
 * HccRepositoryConfig
 *
 * @author hushengjun
 * @date 2023/4/6
 */
@Configuration
@MapperScan("com.hcc.repository.test.mapper")
public class HccRepositoryConfig {
}
