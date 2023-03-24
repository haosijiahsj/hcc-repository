package com.hcc.repository.core.conditions.update;

import com.hcc.repository.core.conditions.AbstractLambdaCondition;
import com.hcc.repository.core.conditions.interfaces.SFunction;
import com.hcc.repository.core.conditions.interfaces.SetClause;
import com.hcc.repository.core.utils.StrUtils;

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

    public LambdaUpdateCondition(Class<T> entityClass) {
        this();
        super.setEntityClass(entityClass);
    }

    @Override
    public LambdaUpdateCondition<T> set(boolean condition, SFunction<T, ?> column, Object val) {
        if (condition) {
            String columnName = this.getColumnName(column);
            String namedColumnName = this.getNamedColumnName(columnName);
            sqlSets.add(String.format("%s = %s", columnName, ":" + namedColumnName));
            super.putColumnValue(namedColumnName, val);
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

    @Override
    public String getSqlWhere() {
        String lastSql = getLastSql();

        return getSegmentContainer().getSqlSegment()
                + (StrUtils.isEmpty(lastSql) ? "" : " " + lastSql);
    }

    @Override
    public String getSqlCount() {
        throw new UnsupportedOperationException();
    }

    @Override
    public String getSqlDelete() {
        throw new UnsupportedOperationException();
    }

    @Override
    public String getSqlQuery() {
        throw new UnsupportedOperationException();
    }

}
