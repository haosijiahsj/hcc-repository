package com.hcc.repository.extension.interceptor.logicdelete;

import javafx.beans.binding.IntegerExpression;
import net.sf.jsqlparser.expression.Expression;
import net.sf.jsqlparser.expression.LongValue;

/**
 * 逻辑删除处理器
 *
 * @author hushengjun
 * @date 2023/5/1
 */
public interface LogicDeleteHandler {

    /**
     * 逻辑删除字段
     * @return
     */
    default String logicDelColumnName() {
        return "deleted";
    }

    /**
     * 逻辑删除值
     * @return
     */
    Expression logicDelColumnValue();

    /**
     * 逻辑未删除值
     * @return
     */
    Expression logicNotDelColumnValue();

    /**
     * 该表是否忽略逻辑删除处理
     * @param tableName
     * @return
     */
    default boolean ignoreTable(String tableName) {
        return false;
    }

}
