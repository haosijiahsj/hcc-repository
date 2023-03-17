package com.hcc.repository.core.condition.update;

import com.hcc.repository.core.condition.AbstractLambdaCondition;
import com.hcc.repository.core.condition.interfaces.SFunction;
import com.hcc.repository.core.condition.interfaces.SetClause;

import java.util.ArrayList;
import java.util.List;

/**
 * LambdaUpdateConditions
 *
 * @author hushengjun
 * @date 2023/3/3
 */
public class LambdaUpdateCondition<T> extends AbstractLambdaCondition<T, LambdaUpdateCondition<T>> implements SetClause<LambdaUpdateCondition<T>, SFunction<T, ?>> {

    private final List<String> sqlSets;

    public LambdaUpdateCondition() {
        super.init();
        sqlSets = new ArrayList<>();
    }

    @Override
    public LambdaUpdateCondition<T> set(boolean condition, SFunction<T, ?> column, Object val) {
        if (condition) {
            String columnName = this.getColumnName(column);
            sqlSets.add(String.format("%s = %s", columnName, ":" + column));
            super.putColumnValue(columnName, val);
        }
        return typeThis;
    }

    @Override
    public LambdaUpdateCondition<T> setSql(boolean condition, String setSql) {
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
