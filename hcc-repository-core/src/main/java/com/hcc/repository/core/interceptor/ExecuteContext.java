package com.hcc.repository.core.interceptor;

import com.hcc.repository.core.constants.MethodNameEnum;
import com.hcc.repository.core.constants.SqlTypeEnum;
import lombok.Data;

/**
 * ExecuteContext
 *
 * @author hushengjun
 * @date 2023/3/23
 */
@Data
public class ExecuteContext {

    private SqlTypeEnum sqlType;
    private MethodNameEnum methodName;
    private String sql;
    private Object[] args;

}
