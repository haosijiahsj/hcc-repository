package com.hcc.repository.core.jdbc;

import com.hcc.repository.core.utils.JdbcUtils;

import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;

/**
 * 结果映射器
 *
 * @author hushengjun
 * @date 2023/4/26
 */
@FunctionalInterface
public interface ResultMapper<T> {

    /**
     * 获取列名，优先获取别名
     * @param rsMetaData
     * @param columnIndex
     * @return
     * @throws SQLException
     */
    default String getColumnName(ResultSetMetaData rsMetaData, int columnIndex) {
        try {
            return JdbcUtils.lookupColumnName(rsMetaData, columnIndex);
        } catch (SQLException e) {
            throw new IllegalStateException(String.format("第%s列获取列名失败！", columnIndex));
        }
    }

    /**
     * 结果映射方法
     * @param rs
     * @param rowNum
     * @return
     * @throws SQLException
     */
    T resultMap(ResultSet rs, int rowNum) throws SQLException;

}
