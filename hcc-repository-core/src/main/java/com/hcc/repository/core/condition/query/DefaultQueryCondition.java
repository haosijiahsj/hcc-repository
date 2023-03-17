package com.hcc.repository.core.condition.query;

import com.hcc.repository.core.condition.AbstractCondition;
import com.hcc.repository.core.condition.interfaces.SelectClause;
import com.hcc.repository.core.condition.update.DefaultUpdateCondition;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * QueryConditions
 *
 * @author hushengjun
 * @date 2023/3/3
 */
public class DefaultQueryCondition<T> extends AbstractCondition<T, String, DefaultQueryCondition<T>> implements SelectClause<DefaultQueryCondition<T>, T, String> {

    private final List<String> selectColumns;

    public DefaultQueryCondition() {
        super.init();
        selectColumns = new ArrayList<>(32);
    }

    @Override
    public DefaultQueryCondition<T> select(String...columns) {
        selectColumns.addAll(Arrays.asList(columns));
        return typeThis;
    }

    @Override
    public String getSqlSelect() {
        return "SELECT " + (selectColumns.isEmpty() ? "*" : String.join(", ", selectColumns));
    }

}
