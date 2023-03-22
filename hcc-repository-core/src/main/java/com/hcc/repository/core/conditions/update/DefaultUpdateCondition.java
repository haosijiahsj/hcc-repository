package com.hcc.repository.core.conditions.update;

import com.hcc.repository.core.conditions.AbstractCondition;
import com.hcc.repository.core.conditions.interfaces.SetClause;
import com.hcc.repository.core.metadata.TableInfo;
import com.hcc.repository.core.metadata.TableInfoHelper;
import com.hcc.repository.core.utils.StrUtils;

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

    public DefaultUpdateCondition(Class<T> entityClass) {
        this();
        super.setEntityClass(entityClass);
    }

    @Override
    public DefaultUpdateCondition<T> set(boolean condition, String column, Object val) {
        if (condition) {
            String namedColumnName = this.getNamedColumnName(column);
            sqlSets.add(String.format("%s = %s", column, ":" + namedColumnName));
            super.putColumnValue(namedColumnName, val);
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
        return sqlSets.isEmpty() ? " " : ("SET " + String.join(", ", sqlSets));
    }

    @Override
    public String getSqlWhere() {
        String lastSql = getLastSql();

        return getSegmentContainer().getSqlSegment()
                + (StrUtils.isEmpty(lastSql) ? "" : " " + lastSql);
    }

}
