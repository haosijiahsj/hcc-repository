package com.hcc.repository.core.interceptor;

import com.hcc.repository.core.constants.SqlTypeEnum;
import lombok.Data;

import java.util.Map;

/**
 * ExecuteContext
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
     * 是否具名sql
     */
    private boolean isNamedSql;
    /**
     * sql
     */
    private String sql;
    /**
     * map类型，数组
     */
    private Object sqlParameter;

}
