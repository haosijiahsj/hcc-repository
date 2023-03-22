package com.hcc.repository.core.jdbc;

import com.hcc.repository.core.interceptor.Interceptor;
import org.springframework.jdbc.core.JdbcOperations;
import org.springframework.jdbc.core.PreparedStatementCreator;
import org.springframework.jdbc.core.PreparedStatementCreatorFactory;
import org.springframework.jdbc.core.SqlParameter;
import org.springframework.jdbc.core.namedparam.NamedParameterJdbcTemplate;
import org.springframework.jdbc.core.namedparam.NamedParameterUtils;
import org.springframework.jdbc.core.namedparam.ParsedSql;
import org.springframework.jdbc.core.namedparam.SqlParameterSource;

import javax.sql.DataSource;
import java.util.List;

/**
 * NamedParameterJdbcTemplateDelegate
 *
 * @author hushengjun
 * @date 2023/3/22
 */
public class NamedParameterJdbcTemplateDelegate extends NamedParameterJdbcTemplate {

    private List<Interceptor> interceptors;

    public NamedParameterJdbcTemplateDelegate(DataSource dataSource) {
        super(dataSource);
    }

    public NamedParameterJdbcTemplateDelegate(JdbcOperations classicJdbcTemplate) {
        super(classicJdbcTemplate);
    }

    @Override
    protected PreparedStatementCreator getPreparedStatementCreator(String sql, SqlParameterSource paramSource) {
        ParsedSql parsedSql = this.getParsedSql(sql);

        // TODO 这里拿到了最终的sql和参数
        String sqlToUse = NamedParameterUtils.substituteNamedParameters(parsedSql, paramSource);
        Object[] params = NamedParameterUtils.buildValueArray(parsedSql, paramSource, null);

        List<SqlParameter> declaredParameters = NamedParameterUtils.buildSqlParameterList(parsedSql, paramSource);
        PreparedStatementCreatorFactory pscf = new PreparedStatementCreatorFactory(sqlToUse, declaredParameters);
        return pscf.newPreparedStatementCreator(params);
    }
}
