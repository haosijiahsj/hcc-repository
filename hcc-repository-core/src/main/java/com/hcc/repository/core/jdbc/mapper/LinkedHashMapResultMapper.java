package com.hcc.repository.core.jdbc.mapper;

import java.util.LinkedHashMap;
import java.util.Map;

/**
 * LinkedHashMap映射器
 *
 * @author hushengjun
 * @date 2023/4/26
 */
public class LinkedHashMapResultMapper extends HashMapResultMapper {

    @Override
    protected Map<String, Object> newMapInstance(int columnCount) {
        return new LinkedHashMap<>(columnCount);
    }

}
