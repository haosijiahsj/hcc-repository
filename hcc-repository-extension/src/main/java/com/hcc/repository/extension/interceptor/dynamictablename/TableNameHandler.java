package com.hcc.repository.extension.interceptor.dynamictablename;

/**
 * TableNameHandler
 *
 * @author hushengjun
 * @date 2023/4/29
 */
@FunctionalInterface
public interface TableNameHandler {

    /**
     * 修改表名的方法
     * @param originalTableName
     * @param curSql
     * @return
     */
    String tableName(String originalTableName, String curSql);

}
