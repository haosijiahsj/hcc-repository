package com.hcc.repository.core.proxy;

import com.hcc.repository.core.jdbc.JdbcOperations;
import com.hcc.repository.core.spring.config.RepositoryConfiguration;
import lombok.extern.slf4j.Slf4j;

import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;
import java.util.Arrays;
import java.util.Collection;
import java.util.stream.Collectors;

/**
 * JdbcOperations代理，扩展用
 *
 * @author hushengjun
 * @date 2023/3/26
 */
@Slf4j
public class JdbcOperationsInvocationHandler implements InvocationHandler {

    private final JdbcOperations jdbcOperations;
    private final RepositoryConfiguration configuration;

    public JdbcOperationsInvocationHandler(JdbcOperations jdbcOperations, RepositoryConfiguration configuration) {
        this.jdbcOperations = jdbcOperations;
        this.configuration = configuration;
    }

    @Override
    public Object invoke(Object proxy, Method method, Object[] args) throws Throwable {
        if (method.getName().equals("getJdbcTemplate")
                || method.getName().equals("getNamedParameterJdbcTemplate")) {
            // 这两个方法直接返回
            return method.invoke(jdbcOperations, args);
        }

        long begin = System.currentTimeMillis();

        // 执行sql
        Object executeResult = method.invoke(jdbcOperations, args);

        if (configuration.isPrintSqlLog()) {
            this.printSqlLog(args[0].toString(), args[1], executeResult, System.currentTimeMillis() - begin);
        }

        return executeResult;
    }

    /**
     * 打印sql
     * @param sql
     * @param sqlArg
     */
    private void printSqlLog(String sql, Object sqlArg, Object result, long totalMs) {
        String sqlParamMsg;
        if (sqlArg instanceof Object[]) {
            String paramStr = Arrays.stream((Object[]) sqlArg)
                    .map(param -> param == null ? "null" : param + "(" + param.getClass().getSimpleName() + ")")
                    .collect(Collectors.joining(", "));
            sqlParamMsg = "==>  Parameters: " + paramStr;
        } else {
            sqlParamMsg = "==>  Parameters: " + sqlArg;
        }

        long total = 0;
        if (result != null) {
            if (result instanceof Collection) {
                total = ((Collection<?>) result).size();
            } else {
                total = 1;
            }
        }

        System.out.println(
                "========== execute sql begin ==========\n"
                + "==>  Preparing:  " + sql + "\n"
                + sqlParamMsg + "\n"
                + "<==       Total: " + total + "\n"
                + String.format("elapsed time: %sms%n", totalMs)
                + "========== execute sql end   =========="
        );
    }

}
