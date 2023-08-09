package com.hcc.repository.core.metadata;

import com.hcc.repository.annotation.Constants;
import com.hcc.repository.annotation.PropSetListener;
import lombok.Data;

import java.util.Collections;
import java.util.List;

/**
 * 实体元数据
 *
 * @author hushengjun
 * @date 2023/3/6
 */
@Data
public class TableInfo {

    /**
     * 表名
     */
    private String tableName;
    /**
     * 列前缀
     */
    private String columnPrefix = "";
    /**
     * 忽略的属性名称
     */
    private List<String> ignorePropNames = Collections.emptyList();
    /**
     * 类class
     */
    private Class<?> clazz;
    /**
     * 属性设置监听器
     */
    private Class<? extends PropSetListener> propSet = Constants.UnknownPropSetListener.class;
    /**
     * id列
     */
    private TableColumnInfo idColumnInfo;
    /**
     * 所有列
     */
    private List<TableColumnInfo> columnInfos;
    /**
     * 是否有id列
     */
    private boolean hasIdColumn = false;
    /**
     * 是否有逻辑删除列
     */
    private boolean hasLogicDeleteColumn = false;

    /**
     * 是否需要PropSetListener处理
     * @return
     */
    public boolean needPropSet() {
        return !Constants.UnknownPropSetListener.class.equals(propSet) && !PropSetListener.class.equals(propSet);
    }

}
