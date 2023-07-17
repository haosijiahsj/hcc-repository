package com.hcc.repository.core.jdbc.batch;


import java.sql.PreparedStatement;
import java.sql.SQLException;

/**
 * BatchPreparedStatementSetter包装
 *
 * @author hushengjun
 * @date 2023/7/17
 */
public class BatchPreparedStatementSetterWrapper<T> implements org.springframework.jdbc.core.BatchPreparedStatementSetter {

    private final BatchPreparedStatementSetter batchPreparedStatementSetter;

    public BatchPreparedStatementSetterWrapper(BatchPreparedStatementSetter batchPreparedStatementSetter) {
        this.batchPreparedStatementSetter = batchPreparedStatementSetter;
    }

    @Override
    public void setValues(PreparedStatement ps, int i) throws SQLException {
        batchPreparedStatementSetter.setValues(ps, i);
    }

    @Override
    public int getBatchSize() {
        return batchPreparedStatementSetter.getBatchSize();
    }

}
