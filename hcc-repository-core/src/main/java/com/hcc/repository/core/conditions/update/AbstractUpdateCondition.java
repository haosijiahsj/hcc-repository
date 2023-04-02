package com.hcc.repository.core.conditions.update;

import com.hcc.repository.core.conditions.AbstractCondition;
import com.hcc.repository.core.conditions.interfaces.SetClause;
import com.hcc.repository.core.constants.SqlKeywordEnum;
import com.hcc.repository.core.constants.StrPool;
import com.hcc.repository.core.metadata.TableInfoHelper;
import com.hcc.repository.core.utils.StrUtils;

import java.util.ArrayList;
import java.util.List;

/**
 * AbstractUpdateCondition
 *
 * @author hushengjun
 * @date 2023/3/25
 */
public class AbstractUpdateCondition<T, R, C extends AbstractCondition<T, R, C>> extends AbstractCondition<T, R, C> implements SetClause<C, R> {

    private List<String> sqlSets;

    @Override
    protected void init() {
        super.init();
        sqlSets = new ArrayList<>(32);
    }

    @Override
    public C set(boolean condition, R column, Object val) {
        if (condition) {
            String columnName = this.getColumnName(column);
            String namedColumnName = this.getNamedColumnName(columnName);
            sqlSets.add(String.format("%s = %s", columnName, ":" + namedColumnName));
            super.putColumnValue(namedColumnName, val);
        }

        return typeThis;
    }

    @Override
    public C setSql(boolean condition, String setSql) {
        if (condition) {
            sqlSets.add(setSql);
        }
        return typeThis;
    }

    private String getSqlSet() {
        return SqlKeywordEnum.SET + StrPool.SPACE + String.join(StrPool.COMMA_SPACE, sqlSets);
    }

    public String getSqlWhere() {
        return getSegmentContainer().getSqlSegmentAfterWhere();
    }

    private String tableName() {
        return TableInfoHelper.getTableName(this.getEntityClass());
    }

    @Override
    public String getExecuteSql() {
        String sqlSet = getSqlSet();
        if (StrUtils.isEmpty(sqlSet)) {
            throw new IllegalArgumentException("没有set的sql片段");
        }

        return StrUtils.joinSpace(
                SqlKeywordEnum.UPDATE.getKeyword(),
                tableName(),
                sqlSet,
                getSqlWhere()
        );
    }

}
