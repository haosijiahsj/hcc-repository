package com.hcc.repository.core.jdbc.mapper;

import com.hcc.repository.core.jdbc.BasicRowMapper;
import org.springframework.jdbc.support.JdbcUtils;

import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.util.HashMap;
import java.util.Map;

/**
 * HashMap映射器
 *
 * @author hushengjun
 * @date 2023/4/26
 */
public class HashMapRowMapper implements BasicRowMapper<Map<String, Object>> {

    @Override
    public Map<String, Object> resultMap(ResultSet rs, int rowNum) throws SQLException {
        ResultSetMetaData rsmd = rs.getMetaData();

        int columnCount = rsmd.getColumnCount();
        Map<String, Object> resultMap = this.newMapInstance(columnCount);
        for (int index = 1; index <= columnCount; index++) {
            resultMap.put(this.getColumnName(rsmd, index), JdbcUtils.getResultSetValue(rs, index));
        }

        return resultMap;
    }

    /**
     * 实例化Map
     * @return
     */
    protected Map<String, Object> newMapInstance(int columnCount) {
        return new HashMap<>(columnCount);
    }

}
