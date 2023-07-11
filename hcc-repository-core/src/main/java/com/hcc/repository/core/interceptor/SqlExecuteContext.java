package com.hcc.repository.core.interceptor;

import com.hcc.repository.core.constants.SqlTypeEnum;
import lombok.Data;

import java.util.function.Supplier;

/**
 * sql执行上下文
 *
 * @author hushengjun
 * @date 2023/3/23
 */
@Data
public class SqlExecuteContext {

    /**
     * sql类型
     */
    private SqlTypeEnum sqlType;
    /**
     * sql
     */
    private String sql;
    /**
     * sql参数
     */
    private Object[] sqlParameters;
    /**
     * 是否要中断执行，仅在beforeExecute方法生效，后续拦截器也不会执行
     */
    private boolean abortExecute = false;
    /**
     * 中间值提供者
     */
    private Supplier<Object> returnValueSupplier = () -> null;

}
