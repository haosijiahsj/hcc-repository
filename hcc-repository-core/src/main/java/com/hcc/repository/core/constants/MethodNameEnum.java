package com.hcc.repository.core.constants;

import com.hcc.repository.core.handler.AbstractMethodHandler;
import com.hcc.repository.core.handler.DeleteMethodHandler;
import com.hcc.repository.core.handler.InsertMethodHandler;
import com.hcc.repository.core.handler.UpdateMethodHandler;

/**
 * MethodNameConstants
 *
 * @author hushengjun
 * @date 2023/3/6
 */
public enum MethodNameEnum {
    INSERT("insert", "插入数据", InsertMethodHandler.class,  "INSERT INTO %s VALUES (%s)"),
    BATCH_INSERT("batchInsert", "批量插入数据", InsertMethodHandler.class, "INSERT INTO %s VALUES %s"),

    DELETE_BY_ID("deleteById", "通过id删除数据", UpdateMethodHandler.class, "DELETE FROM %s WHERE %s = %s"),
    DELETE_BY_IDS("deleteByIds", "通过批量id删除数据", UpdateMethodHandler.class, "DELETE FROM %s WHERE %s IN (%s)"),
    DELETE("delete", "根据条件删除数据", DeleteMethodHandler.class, "DELETE FROM %s %s"),


    ;
    private String methodName;
    private String desc;
    private Class<? extends AbstractMethodHandler> handlerClass;
    private String script;

    MethodNameEnum(String methodName, String desc, Class<? extends AbstractMethodHandler> handlerClass, String script) {
        this.methodName = methodName;
        this.handlerClass = handlerClass;
        this.desc = desc;
        this.script = script;
    }

    public String getMethodName() {
        return methodName;
    }

    public Class<? extends AbstractMethodHandler> getHandlerClass() {
        return handlerClass;
    }
}
