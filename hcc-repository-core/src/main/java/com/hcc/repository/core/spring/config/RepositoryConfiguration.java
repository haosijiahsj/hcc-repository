package com.hcc.repository.core.spring.config;

import com.hcc.repository.core.interceptor.Interceptor;
import lombok.Data;

import javax.sql.DataSource;
import java.util.List;

/**
 * RepositoryConfiguration
 *
 * @author hushengjun
 * @date 2023/4/5
 */
@Data
public class RepositoryConfiguration {

    private DataSource dataSource;
    private List<Interceptor> interceptors;

}
