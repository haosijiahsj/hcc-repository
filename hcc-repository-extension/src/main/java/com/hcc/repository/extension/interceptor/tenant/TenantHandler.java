package com.hcc.repository.extension.interceptor.tenant;

import net.sf.jsqlparser.expression.Expression;

/**
 * 租户处理器
 *
 * @author hushengjun
 * @date 2023/5/1
 */
@FunctionalInterface
public interface TenantHandler {

    /**
     * 租户字段
     * @return
     */
    default String tenantColumnName() {
        return "tenant_id";
    }

    /**
     * 租户值
     * @return
     */
    Expression getTenantId();

    /**
     * 该表是否忽略多租户处理
     * @param tableName
     * @return
     */
    default boolean ignoreTable(String tableName) {
        return false;
    }

}
