package com.hcc.repository.extension.interceptor;

import com.hcc.repository.core.conditions.ICondition;
import com.hcc.repository.core.constants.MethodNameEnum;
import com.hcc.repository.core.interceptor.SqlExecuteContext;
import com.hcc.repository.core.jdbc.JdbcTemplateProxy;
import lombok.extern.slf4j.Slf4j;

import java.util.Arrays;
import java.util.Collection;
import java.util.stream.Collectors;

/**
 * LogInterceptor
 *
 * @author hushengjun
 * @date 2023/4/9
 */
@Slf4j
public class LogInterceptor implements ExtInterceptor {

    @Override
    public void beforePrepareCondition(MethodNameEnum methodNameEnum, Object[] args) {
        log.info("方法：{}，参数：{}", methodNameEnum.getMethodName(), Arrays.toString(args));
    }

    @Override
    public void afterPrepareCondition(MethodNameEnum methodNameEnum, ICondition<?> condition) {
        log.info("方法：{}，condition sql: {}, condition paramMap: {}", methodNameEnum.getMethodName(), condition.getExecuteSql(),
                condition.getColumnValuePairs());
    }

    @Override
    public void beforeExecute(JdbcTemplateProxy jdbcTemplateProxy, SqlExecuteContext context) {
        log.info("==>  Preparing:  {}", context.getSql());
        if (context.getSqlParameter() instanceof Object[]) {
            String paramStr = Arrays.stream((Object[])context.getSqlParameter())
                    .map(param -> param == null ? "null" : param + "(" + param.getClass().getSimpleName() + ")")
                    .collect(Collectors.joining(", "));
            log.info("==>  Parameters: {}", paramStr);
        } else {
            log.info("==>  Parameters: {}", context.getSqlParameter());
        }
    }

    @Override
    public Object beforeReturn(JdbcTemplateProxy jdbcTemplateProxy, SqlExecuteContext context, Object result) {
        String logMsg = "<==       Total: {}";
        int total = 0;
        if (result != null) {
            if (result instanceof Collection) {
                Collection<?> coll = (Collection<?>) result;
                total = coll.size();
                coll.forEach(o -> log.info("<==         Row: {}", o));
            } else {
                total = 1;
                log.info("<==         Row: {}", result);
            }
        }
        log.info(logMsg, total);
        return result;
    }

}
