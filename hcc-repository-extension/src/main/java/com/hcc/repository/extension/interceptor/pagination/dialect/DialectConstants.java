package com.hcc.repository.extension.interceptor.pagination.dialect;

import com.hcc.repository.core.constants.DbType;

import java.util.EnumMap;
import java.util.Map;

/**
 * DialectConstants
 *
 * @author hushengjun
 * @date 2023/7/10
 */
public class DialectConstants {

    private static final Map<DbType, IDialect> dbTypeDialectMap = new EnumMap<>(DbType.class);

    static {
        dbTypeDialectMap.put(DbType.MYSQL, new MysqlDialect());
        dbTypeDialectMap.put(DbType.POSTGRE_SQL, new PostgreDialect());
    }

    public static IDialect getDialect(DbType dbType) {
        return dbTypeDialectMap.get(dbType);
    }

}
