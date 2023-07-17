package com.hcc.repository.core.jdbc.batch;

import java.sql.PreparedStatement;
import java.sql.SQLException;

/**
 * 替代spring的BatchPreparedStatementSetter
 *
 * @author hushengjun
 * @date 2023/7/17
 */
public interface BatchPreparedStatementSetter {

    /**
     * 设置值
     * @param ps
     * @param index
     */
    void setValues(PreparedStatement ps, int index) throws SQLException;

    /**
     * 数量大小
     * @return
     */
    int getBatchSize();

}
