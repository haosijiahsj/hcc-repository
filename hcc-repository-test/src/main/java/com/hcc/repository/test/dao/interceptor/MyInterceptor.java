package com.hcc.repository.test.dao.interceptor;

import com.hcc.repository.core.interceptor.Interceptor;
import com.hcc.repository.core.interceptor.SqlExecuteContext;
import com.hcc.repository.core.jdbc.JdbcTemplateProxy;
import lombok.extern.slf4j.Slf4j;

/**
 * MyInterceptor
 *
 * @author hushengjun
 * @date 2023/4/7
 */
@Slf4j
public class MyInterceptor implements Interceptor {

    @Override
    public void beforeQuery(JdbcTemplateProxy jdbcTemplateProxy, SqlExecuteContext context) {
        log.info("执行了beforeQuery拦截器");
    }

}
