package com.hcc.repository.core.condition.update;

import com.hcc.repository.core.condition.AbstractCondition;
import com.hcc.repository.core.condition.interfaces.SetClause;

import java.util.ArrayList;
import java.util.List;

/**
 * UpdateConditions
 *
 * @author hushengjun
 * @date 2023/3/3
 */
public class DefaultUpdateCondition<T> extends AbstractCondition<T, String, DefaultUpdateCondition<T>> implements SetClause<DefaultUpdateCondition<T>, String> {

    private final List<String> sqlSets;

    public DefaultUpdateCondition() {
        super.init();
        sqlSets = new ArrayList<>();
    }

    @Override
    public DefaultUpdateCondition<T> set(boolean condition, String column, Object val) {
        if (condition) {
            sqlSets.add(String.format("%s = %s", column, ":" + column));
            super.putColumnValue(column, val);
        }

        return typeThis;
    }

    @Override
    public DefaultUpdateCondition<T> setSql(boolean condition, String setSql) {
        if (condition) {
            sqlSets.add(setSql);
        }
        return typeThis;
    }

    @Override
    public String getSqlSet() {
        return sqlSets.isEmpty() ? "" : ("SET " + String.join(", ", sqlSets));
    }

}
