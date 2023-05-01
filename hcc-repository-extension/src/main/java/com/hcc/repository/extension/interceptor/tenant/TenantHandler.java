package com.hcc.repository.extension.interceptor.tenant;

/**
 * TenantHandler
 *
 * @author hushengjun
 * @date 2023/5/1
 */
public interface TenantHandler {

    /**
     * 租户字段
     * @return
     */
    default String tenantColumn() {
        return "tenant_id";
    }

    /**
     * 该表是否忽略多租户处理
     * @param tableName
     * @return
     */
    default boolean ignoreTable(String tableName) {
        return false;
    }

}
