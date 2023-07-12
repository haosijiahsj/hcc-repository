package com.hcc.repository.extension.interceptor.pagination.dialect;

import com.hcc.repository.core.constants.DbType;
import com.hcc.repository.extension.interceptor.pagination.dialect.handler.MysqlDialect;
import com.hcc.repository.extension.interceptor.pagination.dialect.handler.Oracle12cDialect;
import com.hcc.repository.extension.interceptor.pagination.dialect.handler.OracleDialect;
import com.hcc.repository.extension.interceptor.pagination.dialect.handler.PostgreDialect;
import com.hcc.repository.extension.interceptor.pagination.dialect.handler.SQLServerDialect;

import java.util.EnumMap;
import java.util.HashMap;
import java.util.Map;

/**
 * 方言工厂
 *
 * @author hushengjun
 * @date 2023/7/10
 */
public class DialectFactory {

    private static final Map<DbType, IDialect> dbTypeDialectMap = new EnumMap<>(DbType.class);
    private static final Map<String, IDialect> customerDialectMap = new HashMap<>();

    static {
        dbTypeDialectMap.put(DbType.MYSQL, new MysqlDialect());
        dbTypeDialectMap.put(DbType.POSTGRE_SQL, new PostgreDialect());
        dbTypeDialectMap.put(DbType.ORACLE, new OracleDialect());
        dbTypeDialectMap.put(DbType.ORACLE_12C, new Oracle12cDialect());
        dbTypeDialectMap.put(DbType.SQL_SERVER, new SQLServerDialect());
    }

    public static void registerDialect(String dbTypeName, IDialect dialect) {
        customerDialectMap.put(dbTypeName, dialect);
    }

    /**
     * 获取方言
     * @param dbType
     * @return
     */
    public static IDialect getDialect(DbType dbType) {
        return dbTypeDialectMap.get(dbType);
    }

    /**
     * 获取方言
     * @param dbTypeName
     * @return
     */
    public static IDialect getCustomerDialect(String dbTypeName) {
        return customerDialectMap.get(dbTypeName);
    }

}