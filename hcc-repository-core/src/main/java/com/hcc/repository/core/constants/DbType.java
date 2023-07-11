package com.hcc.repository.core.constants;

/**
 * DbType
 *
 * @author hushengjun
 * @date 2023/4/8
 */
public enum DbType {

    MYSQL("mysql", "mysql数据库"),
    POSTGRE_SQL("postgresql", "postgresql数据库"),
    UNKNOWN("unknown", "未知数据库");

    String name;
    String desc;

    DbType(String name, String desc) {
        this.name = name;
        this.desc = desc;
    }

    public String getName() {
        return name;
    }

    public String getDesc() {
        return desc;
    }

}
