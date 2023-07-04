package com.hcc.repository.core.jdbc.batch;

import com.hcc.repository.core.utils.CollUtils;
import lombok.extern.slf4j.Slf4j;

import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.util.Map;

/**
 * MapPreparedStatementObjectSetter
 *
 * @author hushengjun
 * @date 2023/7/4
 */
@Slf4j
public class MapPreparedStatementObjectSetter implements PreparedStatementObjectSetter<Map<String, Object>> {

    private final Map<Integer, String> indexColumnNameMap;

    public MapPreparedStatementObjectSetter(Map<Integer, String> indexColumnNameMap) {
        this.indexColumnNameMap = indexColumnNameMap;
    }

    @Override
    public void setValues(PreparedStatement ps, Map<String, Object> map) throws SQLException {
        if (CollUtils.isEmpty(indexColumnNameMap)) {
            return;
        }
        int size = indexColumnNameMap.size();
        for (int i = 1; i <= size; i++) {
            String columnName = indexColumnNameMap.get(i);
            if (columnName == null) {
                ps.setObject(i, null);
                log.warn("批量插入时，第{}列未获取到列名", i);
                continue;
            }

            ps.setObject(i, map.get(columnName));
        }
    }

}
