package com.hcc.repository.core.constants;

import com.hcc.repository.core.handler.AbstractMethodHandler;
import com.hcc.repository.core.handler.delete.DeleteByIdHandler;
import com.hcc.repository.core.handler.delete.DeleteByIdsHandler;
import com.hcc.repository.core.handler.delete.DeleteHandler;
import com.hcc.repository.core.handler.insert.BatchInsertHandler;
import com.hcc.repository.core.handler.insert.InsertHandler;
import com.hcc.repository.core.handler.select.SelectByIdHandler;
import com.hcc.repository.core.handler.select.SelectByIdsHandler;
import com.hcc.repository.core.handler.select.SelectCountHandler;
import com.hcc.repository.core.handler.select.SelectIdsHandler;
import com.hcc.repository.core.handler.select.SelectListHandler;
import com.hcc.repository.core.handler.select.SelectMapsHandler;
import com.hcc.repository.core.handler.select.SelectOneHandler;
import com.hcc.repository.core.handler.update.UpdateByIdHandler;
import com.hcc.repository.core.handler.update.UpdateHandler;

import java.util.Arrays;
import java.util.List;

/**
 * MethodNameConstants
 *
 * @author hushengjun
 * @date 2023/3/6
 */
public enum MethodNameEnum {

    // 增
    INSERT("insert", "插入数据", InsertHandler.class),
    BATCH_INSERT("batchInsert", "批量插入数据", BatchInsertHandler.class),

    // 删
    DELETE_BY_ID("deleteById", "通过id删除数据", DeleteByIdHandler.class),
    DELETE_BY_IDS("deleteByIds", "通过批量id删除数据", DeleteByIdsHandler.class),
    DELETE("delete", "根据条件删除数据", DeleteHandler.class),

    // 改
    UPDATE_BY_ID("updateById", "通过id更新", UpdateByIdHandler.class),
    UPDATE("update", "更新", UpdateHandler.class),

    // 查
    SELECT_BY_ID("selectById", "通过id查询", SelectByIdHandler.class),
    SELECT_BY_IDS("selectByIds", "通过id列表查询", SelectByIdsHandler.class),
    SELECT_COUNT("selectCount", "查询数量", SelectCountHandler.class),
    SELECT_IDS("selectIds", "查询id列表", SelectIdsHandler.class),
    SELECT_LIST("selectList", "查询列表", SelectListHandler.class),
    SELECT_MAPS("selectMaps", "查询map列表", SelectMapsHandler.class),
    SELECT_ONE("selectOne", "查询查询", SelectOneHandler.class),

    ;

    private static final List<MethodNameEnum> INSERT_METHODS;
    private static final List<MethodNameEnum> DELETE_METHODS;
    private static final List<MethodNameEnum> UPDATE_METHODS;
    private static final List<MethodNameEnum> SELECT_METHODS;

    static {
        INSERT_METHODS = Arrays.asList(INSERT, BATCH_INSERT);
        DELETE_METHODS = Arrays.asList(DELETE, DELETE_BY_ID, DELETE_BY_IDS);
        UPDATE_METHODS = Arrays.asList(UPDATE, UPDATE_BY_ID);
        SELECT_METHODS = Arrays.asList(SELECT_BY_ID, SELECT_BY_IDS, SELECT_COUNT, SELECT_IDS, SELECT_LIST, SELECT_MAPS, SELECT_ONE);
    }
    private String methodName;
    private String desc;
    private Class<? extends AbstractMethodHandler> handlerClass;

    MethodNameEnum(String methodName, String desc, Class<? extends AbstractMethodHandler> handlerClass) {
        this.methodName = methodName;
        this.handlerClass = handlerClass;
        this.desc = desc;
    }

    public String getMethodName() {
        return methodName;
    }

    public Class<? extends AbstractMethodHandler> getHandlerClass() {
        return handlerClass;
    }

    public static MethodNameEnum get(String methodName) {
        MethodNameEnum[] values = MethodNameEnum.values();
        for (MethodNameEnum methodNameEnum : values) {
            if (methodNameEnum.getMethodName().equals(methodName)) {
                return methodNameEnum;
            }
        }

        return null;
    }

    public static boolean isC(MethodNameEnum methodNameEnum) {
        return INSERT_METHODS.contains(methodNameEnum);
    }
    public static boolean isR(MethodNameEnum methodNameEnum) {
        return SELECT_METHODS.contains(methodNameEnum);
    }
    public static boolean isU(MethodNameEnum methodNameEnum) {
        return UPDATE_METHODS.contains(methodNameEnum);
    }
    public static boolean isD(MethodNameEnum methodNameEnum) {
        return DELETE_METHODS.contains(methodNameEnum);
    }

}
