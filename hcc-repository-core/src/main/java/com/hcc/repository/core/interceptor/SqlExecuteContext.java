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
     * 是否要中断执行
     */
    private boolean abortExecute = false;
    /**
     * abortExecute=true时的默认返回值
     */
    private Supplier<Object> returnValueSupplier = () -> null;

}
