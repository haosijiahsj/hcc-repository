package com.hcc.repository.core.conditions.query;

import com.hcc.repository.core.conditions.AbstractCondition;
import com.hcc.repository.core.conditions.interfaces.SelectClause;
import com.hcc.repository.core.metadata.TableColumnInfo;
import com.hcc.repository.core.metadata.TableInfo;
import com.hcc.repository.core.metadata.TableInfoHelper;
import com.hcc.repository.core.utils.StrUtils;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.function.Predicate;
import java.util.stream.Collectors;

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

    public DefaultQueryCondition(Class<T> entityClass) {
        this();
        super.setEntityClass(entityClass);
    }

    public List<String> getSelectColumns() {
        return selectColumns;
    }

    @Override
    public DefaultQueryCondition<T> select(String...columns) {
        selectColumns.addAll(Arrays.asList(columns));
        return typeThis;
    }

    @Override
    public DefaultQueryCondition<T> select(Class<T> entityClass, Predicate<TableColumnInfo> predicate) {
        super.setEntityClass(entityClass);
        List<String> columnNames = TableInfoHelper.getColumnInfos(getEntityClass())
                .stream()
                .filter(predicate)
                .map(TableColumnInfo::getColumnName)
                .collect(Collectors.toList());
        selectColumns.addAll(columnNames);
        return typeThis;
    }

    @Override
    public String getSqlSelect() {
        return "SELECT " + (selectColumns.isEmpty() ? "*" : String.join(", ", selectColumns));
    }

    @Override
    public String getSqlWhere() {
        String lastSql = getLastSql();

        return getSegmentContainer().getSqlSegment()
                + (StrUtils.isEmpty(lastSql) ? "" : " " + lastSql);
    }

    @Override
    public String getSqlUpdate() {
        throw new UnsupportedOperationException();
    }

}
