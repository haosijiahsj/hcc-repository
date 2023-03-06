package com.hcc.repository.core.constants;

/**
 * MethodNameConstants
 *
 * @author hushengjun
 * @date 2023/3/6
 */
public enum MethodNameEnum {
    INSERT("insert", "插入数据", "INSERT INTO %s VALUES (%s)"),
    BATCH_INSERT("batchInsert", "批量插入数据", "INSERT INTO %s VALUES %s"),

    DELETE_BY_ID("deleteById", "通过id删除数据", "DELETE FROM %s WHERE %s = %s"),
    DELETE_BY_IDS("deleteByIds", "通过批量id删除数据", "DELETE FROM %s WHERE %s IN (%s)"),
    DELETE("delete", "根据条件删除数据", "DELETE FROM %s %s"),


    ;
    private String methodName;
    private String desc;
    private String script;

    MethodNameEnum(String methodName, String desc, String script) {
        this.methodName = methodName;
        this.desc = desc;
        this.script = script;
    }

}
