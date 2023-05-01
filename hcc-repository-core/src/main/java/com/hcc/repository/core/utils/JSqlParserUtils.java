package com.hcc.repository.core.utils;

import com.hcc.repository.core.constants.SqlTypeEnum;
import net.sf.jsqlparser.JSQLParserException;
import net.sf.jsqlparser.parser.CCJSqlParserUtil;
import net.sf.jsqlparser.statement.Statement;
import net.sf.jsqlparser.statement.delete.Delete;
import net.sf.jsqlparser.statement.insert.Insert;
import net.sf.jsqlparser.statement.select.Select;
import net.sf.jsqlparser.statement.update.Update;

/**
 * sql解析工具
 *
 * @author hushengjun
 * @date 2023/5/1
 */
public class JSqlParserUtils {

    public static SqlTypeEnum getSqlType(String sql) {
        Statement statement;
        try {
            statement = CCJSqlParserUtil.parse(sql);
        } catch (JSQLParserException e) {
            throw new IllegalArgumentException(String.format("sql: %s解析失败", sql));
        }
        if (statement instanceof Insert) {
            return SqlTypeEnum.INSERT;
        } else if (statement instanceof Delete) {
            return SqlTypeEnum.DELETE;
        } else if (statement instanceof Update) {
            return SqlTypeEnum.UPDATE;
        } else if (statement instanceof Select) {
            return SqlTypeEnum.SELECT;
        }

        throw new IllegalArgumentException(String.format("sql: %s解析失败", sql));
    }

}
