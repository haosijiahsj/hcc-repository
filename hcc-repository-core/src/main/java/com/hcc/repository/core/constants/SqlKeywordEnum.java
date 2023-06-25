package com.hcc.repository.core.constants;

/**
 * SqlKeyWordEnum
 *
 * @author hushengjun
 * @date 2023/3/16
 */
public enum SqlKeywordEnum {

    AND("AND"),
    OR("OR"),
    NOT("NOT"),
    IN("IN"),
    NOT_IN("NOT IN"),
    LIKE("LIKE"),
    NOT_LIKE("NOT LIKE"),
    EQ("="),
    NE("<>"),
    GT(">"),
    GE(">="),
    LT("<"),
    LE("<="),
    IS_NULL("IS NULL"),
    IS_NOT_NULL("IS NOT NULL"),
    GROUP_BY("GROUP BY"),
    HAVING("HAVING"),
    ORDER_BY("ORDER BY"),
    EXISTS("EXISTS"),
    NOT_EXISTS("NOT EXISTS"),
    BETWEEN("BETWEEN"),
    NOT_BETWEEN("NOT BETWEEN"),
    ASC("ASC"),
    DESC("DESC"),

    INSERT_INTO("INSERT INTO"),
    VALUES("VALUES"),
    DELETE_FROM("DELETE FROM"),
    UPDATE("UPDATE"),
    SELECT("SELECT"),
    WHERE("WHERE"),
    FROM("FROM"),
    SET("SET"),
    COUNT("COUNT(*)"),
    DISTINCT("DISTINCT");

    private final String keyword;

    SqlKeywordEnum(String keyword) {
        this.keyword = keyword;
    }

    public String getKeyword() {
        return keyword;
    }
}
