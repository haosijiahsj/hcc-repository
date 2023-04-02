package com.hcc.repository.core.conditions.query;

import com.hcc.repository.core.conditions.AbstractCondition;
import com.hcc.repository.core.conditions.interfaces.SelectClause;
import com.hcc.repository.core.constants.ExecuteSqlTypeEnum;
import com.hcc.repository.core.constants.SqlKeywordEnum;
import com.hcc.repository.core.constants.StrPool;
import com.hcc.repository.core.metadata.TableColumnInfo;
import com.hcc.repository.core.metadata.TableInfoHelper;
import com.hcc.repository.core.utils.StrUtils;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.function.Predicate;
import java.util.stream.Collectors;

/**
 * AbstractQueryCondition
 *
 * @author hushengjun
 * @date 2023/3/25
 */
public class AbstractQueryCondition<T, R, C extends AbstractCondition<T, R, C>> extends AbstractCondition<T, R, C> implements SelectClause<C, T, R> {

    private List<String> selectColumns;
    protected ExecuteSqlTypeEnum executeSqlType;

    @Override
    protected void init() {
        super.init();
        selectColumns = new ArrayList<>(32);
        // 默认是select语句
        this.executeSqlType = ExecuteSqlTypeEnum.SELECT;
    }

    public C setExecuteSqlType(ExecuteSqlTypeEnum selectSqlType) {
        this.executeSqlType = selectSqlType;
        return typeThis;
    }

    public List<String> getSelectColumns() {
        return selectColumns;
    }

    @Override
    @SafeVarargs
    public final C select(R...columns) {
        selectColumns.addAll(Arrays.stream(columns).map(this::getColumnName).collect(Collectors.toList()));
        return typeThis;
    }

    @Override
    public C select(Class<T> entityClass, Predicate<TableColumnInfo> predicate) {
        super.setEntityClass(entityClass);
        List<String> columnNames = TableInfoHelper.getColumnInfos(getEntityClass())
                .stream()
                .filter(predicate)
                .map(TableColumnInfo::getColumnName)
                .collect(Collectors.toList());
        selectColumns.addAll(columnNames);
        return typeThis;
    }

    private String getSqlSelect() {
        String sqlSelect;
        if (selectColumns.isEmpty()) {
            sqlSelect = TableInfoHelper.getColumnInfos(entityClass).stream()
                    .map(TableColumnInfo::getColumnName)
                    .collect(Collectors.joining(StrPool.COMMA_SPACE));
        } else {
            sqlSelect = String.join(StrPool.COMMA_SPACE, selectColumns);
        }

        return SqlKeywordEnum.SELECT.getKeyword() + StrPool.SPACE + sqlSelect;
    }

    public String getSqlWhere() {
        return getSegmentContainer().getSqlSegmentAfterWhere();
    }

    private String tableName() {
        return TableInfoHelper.getTableName(this.getEntityClass());
    }

    /**
     * 获取删除sql
     * @return
     */
    public String getSqlDelete() {
        return StrUtils.joinSpace(
                SqlKeywordEnum.DELETE_FROM.getKeyword(),
                tableName(),
                getSqlWhere()
        );
    }

    /**
     * 获取查询sql
     * @return
     */
    public String getSqlQuery() {
        return StrUtils.joinSpace(
                getSqlSelect(),
                SqlKeywordEnum.FROM.getKeyword(),
                tableName(),
                getSqlWhere()
        );
    }

    /**
     * 获取统计sql
     * @return
     */
    public String getSqlCount() {
        return StrUtils.joinSpace(
                SqlKeywordEnum.SELECT.getKeyword(),
                SqlKeywordEnum.COUNT.getKeyword(),
                SqlKeywordEnum.FROM.getKeyword(),
                tableName(),
                getSqlWhere()
        );
    }

    @Override
    public String getExecuteSql() {
        if (ExecuteSqlTypeEnum.SELECT.equals(executeSqlType)) {
            return this.getSqlQuery();
        } else if (ExecuteSqlTypeEnum.SELECT_COUNT.equals(executeSqlType)) {
            return this.getSqlCount();
        } else if (ExecuteSqlTypeEnum.DELETE.equals(executeSqlType)) {
            return this.getSqlDelete();
        }

        throw new UnsupportedOperationException();
    }
}