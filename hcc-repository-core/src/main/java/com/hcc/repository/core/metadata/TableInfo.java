package com.hcc.repository.core.metadata;

import lombok.Data;

import java.util.Collections;
import java.util.List;

/**
 * TableInfo
 *
 * @author hushengjun
 * @date 2023/3/6
 */
@Data
public class TableInfo {

    private String tableName;
    private String columnPrefix = "";
    private List<String> ignorePropertyNames = Collections.emptyList();
    private Class<?> clazz;
    private TableColumnInfo idColumnInfo;
    private List<TableColumnInfo> columnInfos;
    private boolean hasIdColumn = false;
    private boolean hasLogicDeleteColumn = false;

}
