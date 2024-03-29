package com.hcc.repository.core.constants;

import com.hcc.repository.core.handler.AbstractMethodHandler;
import com.hcc.repository.core.handler.delete.DeleteByIdsHandler;
import com.hcc.repository.core.handler.delete.DeleteByMapHandler;
import com.hcc.repository.core.handler.delete.DeleteHandler;
import com.hcc.repository.core.handler.insert.BatchInsertHandler;
import com.hcc.repository.core.handler.insert.BatchInsertSpliceHandler;
import com.hcc.repository.core.handler.insert.InsertByConditionHandler;
import com.hcc.repository.core.handler.insert.InsertHandler;
import com.hcc.repository.core.handler.select.SelectByIdsHandler;
import com.hcc.repository.core.handler.select.SelectCountHandler;
import com.hcc.repository.core.handler.select.SelectIdsHandler;
import com.hcc.repository.core.handler.select.SelectListByMapHandler;
import com.hcc.repository.core.handler.select.SelectListHandler;
import com.hcc.repository.core.handler.select.SelectMapsHandler;
import com.hcc.repository.core.handler.select.SelectMapsPageHandler;
import com.hcc.repository.core.handler.select.SelectPageHandler;
import com.hcc.repository.core.handler.update.UpdateByIdHandler;
import com.hcc.repository.core.handler.update.UpdateEntityHandler;
import com.hcc.repository.core.handler.update.UpdateHandler;

import java.util.ArrayList;
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
    INSERT_BY_CONDITION("insertByCondition", "插入数据", InsertByConditionHandler.class),
    BATCH_INSERT("batchInsert", "批量插入数据", BatchInsertHandler.class),
    BATCH_INSERT_SPLICE("batchInsertSplice", "拼接式批量插入数据", BatchInsertSpliceHandler.class),

    // 删
    DELETE_BY_ID("deleteById", "通过id删除数据", null),
    DELETE_BY_IDS("deleteByIds", "通过批量id删除数据", DeleteByIdsHandler.class),
    DELETE("delete", "根据条件删除数据", DeleteHandler.class),
    DELETE_BY_MAP("deleteByMap", "根据map删除数据", DeleteByMapHandler.class),

    // 改
    UPDATE_BY_ID("updateById", "通过id更新", UpdateByIdHandler.class),
    UPDATE_ENTITY("updateEntity", "通过condition更新实体", UpdateEntityHandler.class),
    UPDATE("update", "更新", UpdateHandler.class),

    // 查
    SELECT_BY_ID("selectById", "通过id查询", null),
    SELECT_BY_IDS("selectByIds", "通过id列表查询", SelectByIdsHandler.class),
    SELECT_COUNT("selectCount", "查询数量", SelectCountHandler.class),
    SELECT_IDS("selectIds", "查询id列表", SelectIdsHandler.class),
    SELECT_LIST("selectList", "查询列表", SelectListHandler.class),
    SELECT_MAPS("selectMaps", "查询map列表", SelectMapsHandler.class),
    SELECT_ONE("selectOne", "查询单个", null),
    SELECT_LIST_BY_MAP("selectListByMap", "通过map查询", SelectListByMapHandler.class),
    SELECT_PAGE("selectPage", "分页查询", SelectPageHandler.class),
    SELECT_MAPS_PAGE("selectMapsPage", "分页查询map列表", SelectMapsPageHandler.class),

    ;

    private static final List<MethodNameEnum> INSERT_METHODS = new ArrayList<>();
    private static final List<MethodNameEnum> DELETE_METHODS = new ArrayList<>();
    private static final List<MethodNameEnum> UPDATE_METHODS = new ArrayList<>();
    private static final List<MethodNameEnum> SELECT_METHODS = new ArrayList<>();

    static {
        for (MethodNameEnum value : MethodNameEnum.values()) {
            if (value.name().contains(SqlTypeEnum.INSERT.name())) {
                INSERT_METHODS.add(value);
            } else if (value.name().contains(SqlTypeEnum.DELETE.name())) {
                DELETE_METHODS.add(value);
            } else if (value.name().contains(SqlTypeEnum.UPDATE.name())) {
                UPDATE_METHODS.add(value);
            } else if (value.name().contains(SqlTypeEnum.SELECT.name())) {
                SELECT_METHODS.add(value);
            }
        }
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

    public SqlTypeEnum getSqlType() {
        if (INSERT_METHODS.contains(this)) {
            return SqlTypeEnum.INSERT;
        }
        else if (SELECT_METHODS.contains(this)) {
            return SqlTypeEnum.SELECT;
        }
        else if (UPDATE_METHODS.contains(this)) {
            return SqlTypeEnum.UPDATE;
        }
        else if (DELETE_METHODS.contains(this)) {
            return SqlTypeEnum.DELETE;
        }

        throw new IllegalArgumentException("无法判断sql类型");
    }

}
