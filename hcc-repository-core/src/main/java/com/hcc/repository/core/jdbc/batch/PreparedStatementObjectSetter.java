package com.hcc.repository.core.jdbc.batch;

import java.sql.PreparedStatement;
import java.sql.SQLException;

/**
 * 设置对象值
 *
 * @author hushengjun
 * @date 2023/5/1
 */
@FunctionalInterface
public interface PreparedStatementObjectSetter<T> {

    /**
     * 设置值
     * @param ps
     * @param object
     */
    void setValues(PreparedStatement ps, T object) throws SQLException;

}
