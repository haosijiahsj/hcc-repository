package com.hcc.repository.core.metadata;

import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * TableInfoCache
 *
 * @author hushengjun
 * @date 2023/3/17
 */
public class TableInfoCache {

    private static final Map<Class<?>, TableInfo> CACHE = new ConcurrentHashMap<>(128);

    private TableInfoCache() {}

    public static TableInfo get(Class<?> clazz) {
        TableInfo tableInfo = CACHE.get(clazz);
        if (tableInfo != null) {
            return tableInfo;
        }

        tableInfo = new TableInfo();
        // TODO 扫描注解，放入缓存

        CACHE.put(clazz, tableInfo);

        return tableInfo;
    }

    public static TableColumnInfo getTableIdColumnInfo(Class<?> clazz) {
        return get(clazz).getIdColumnInfo();
    }

    public static List<TableColumnInfo> getColumnInfos(Class<?> clazz) {
        return get(clazz).getColumnInfos();
    }

}
