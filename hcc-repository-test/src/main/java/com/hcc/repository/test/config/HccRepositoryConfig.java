package com.hcc.repository.test.config;

import com.hcc.repository.core.spring.EnableRepository;
import com.hcc.repository.extension.interceptor.dynamictablename.DynamicTableNameInterceptor;
import com.hcc.repository.extension.interceptor.logicdelete.EnhanceLogicDeleteInterceptor;
import com.hcc.repository.extension.interceptor.logicdelete.LogicDeleteHandler;
import com.hcc.repository.extension.interceptor.logicdelete.LogicDeleteInterceptor;
import com.hcc.repository.core.constants.DbType;
import com.hcc.repository.extension.interceptor.pagination.PaginationInterceptor;
import com.hcc.repository.extension.interceptor.optimisticlock.OptimisticLockInterceptor;
import com.hcc.repository.extension.interceptor.tenant.TenantInterceptor;
import com.hcc.repository.starter.autoconfigure.RepositoryInterceptor;
import com.hcc.repository.test.dao.interceptor.TestInterceptor;
import net.sf.jsqlparser.expression.Expression;
import net.sf.jsqlparser.expression.LongValue;
import net.sf.jsqlparser.expression.StringValue;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import java.util.UUID;

/**
 * HccRepositoryConfig
 *
 * @author hushengjun
 * @date 2023/4/6
 */
@Configuration
@EnableRepository("com.hcc.repository.test.dao.mapper")
public class HccRepositoryConfig {

    @Bean
    public RepositoryInterceptor repositoryInterceptor() {
        RepositoryInterceptor interceptor = new RepositoryInterceptor();
        interceptor.addInterceptor(new TestInterceptor());
//        interceptor.addInterceptor(new LogicDeleteInterceptor());
        LogicDeleteHandler logicDeleteHandler = new LogicDeleteHandler() {
            @Override
            public Expression logicDelValue() {
                return new StringValue(UUID.randomUUID().toString());
            }

            @Override
            public Expression logicNotDelValue() {
                return new StringValue("0");
            }
        };
        interceptor.addInterceptor(new EnhanceLogicDeleteInterceptor(logicDeleteHandler));
        interceptor.addInterceptor(new OptimisticLockInterceptor());
        interceptor.addInterceptor(new DynamicTableNameInterceptor(((originalTableName, curSql) -> originalTableName)));
//        interceptor.addInterceptor(new TenantInterceptor(() -> new LongValue("1")));
//        interceptor.addInterceptor(new LogInterceptor());
        interceptor.addInterceptor(new PaginationInterceptor(DbType.MYSQL));

        return interceptor;
    }

}
