package com.hcc.repository.extension.interceptor.dynamictablename;

/**
 * TableNameHandler
 *
 * @author hushengjun
 * @date 2023/4/29
 */
@FunctionalInterface
public interface TableNameHandler {

    String tableName(String originalTableName, String curSql);

}
