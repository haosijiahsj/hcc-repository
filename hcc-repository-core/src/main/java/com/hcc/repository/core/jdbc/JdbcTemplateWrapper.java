package com.hcc.repository.core.jdbc;

import org.springframework.jdbc.core.JdbcOperations;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.jdbc.core.PreparedStatementCreator;
import org.springframework.jdbc.core.PreparedStatementCreatorFactory;
import org.springframework.jdbc.core.SqlParameter;
import org.springframework.jdbc.core.namedparam.NamedParameterJdbcTemplate;
import org.springframework.jdbc.core.namedparam.NamedParameterUtils;
import org.springframework.jdbc.core.namedparam.ParsedSql;
import org.springframework.jdbc.core.namedparam.SqlParameterSource;

import javax.sql.DataSource;
import java.util.List;
import java.util.Map;

/**
 * JdbcTemplateWrapper
 *
 * @author hushengjun
 * @date 2023/3/17
 */
public class JdbcTemplateWrapper {

    private JdbcTemplate jdbcTemplate;
    private NamedParameterJdbcTemplateDelegate namedParameterJdbcTemplateDelegate;

    public JdbcTemplateWrapper(JdbcTemplate jdbcTemplate) {
        this.jdbcTemplate = jdbcTemplate;
        this.namedParameterJdbcTemplateDelegate = new NamedParameterJdbcTemplateDelegate(jdbcTemplate);
    }

    public List<Map<String, Object>> namedQueryForList(String sql, Map<String, ?> paramMap) {
        return namedParameterJdbcTemplateDelegate.queryForList(sql, paramMap);
    }

    private static class NamedParameterJdbcTemplateDelegate extends NamedParameterJdbcTemplate {

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

}
