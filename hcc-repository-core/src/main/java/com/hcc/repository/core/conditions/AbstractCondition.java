package com.hcc.repository.core.conditions;

import com.hcc.repository.core.conditions.interfaces.ExtWhereClause;
import com.hcc.repository.core.conditions.interfaces.GroupByClause;
import com.hcc.repository.core.conditions.interfaces.OrderByClause;
import com.hcc.repository.core.conditions.interfaces.WhereClause;
import com.hcc.repository.core.constants.SqlKeywordEnum;
import com.hcc.repository.core.constants.SqlLikeEnum;
import com.hcc.repository.core.utils.CollUtils;
import com.hcc.repository.core.utils.StrUtils;

import java.util.Arrays;
import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * AbstractCondition
 *
 * @author hushengjun
 * @date 2023/3/10
 */
@SuppressWarnings("unchecked")
public abstract class AbstractCondition<T, R, C extends AbstractCondition<T, R, C>> extends ICondition<T>
        implements WhereClause<C, R>, GroupByClause<C, R>, OrderByClause<C, R>, ExtWhereClause<C, R> {

    private AtomicInteger pos;
    protected C typeThis = (C) this;

    private T entity;
    private Class<T> entityClass;
    private String lastSql;
    private Map<String, Object> columnValuePairs;
    private SegmentContainer segmentContainer;

    protected void init() {
        columnValuePairs = new LinkedHashMap<>(16);
        segmentContainer = new SegmentContainer();
        pos = new AtomicInteger(0);
    }

    @Override
    public T getEntity() {
        return entity;
    }

    @Override
    public void setEntityClass(Class entityClass) {
        if (this.entity == null || this.entityClass == null) {
            this.entityClass = entityClass;
        }
    }

    public void setEntity(T entity) {
        this.entity = entity;
    }

    @Override
    public Class<T> getEntityClass() {
        return entity == null ? entityClass : (Class<T>) entity.getClass();
    }

    public void putColumnValue(String column, Object val) {
        columnValuePairs.put(column, val);
    }

    @Override
    public Map<String, Object> getColumnValuePairs() {
        return columnValuePairs;
    }

    public SegmentContainer getSegmentContainer() {
        return segmentContainer;
    }

    protected String getColumnName(R column) {
        return (String) column;
    }

    public String getLastSql() {
        return StrUtils.isEmpty(lastSql) ? "" : lastSql;
    }

    protected String getNamedColumnName(String originalColumnName) {
        return originalColumnName + "#POS_" + pos.incrementAndGet();
    }

    /**
     * 添加条件
     * @param condition
     * @param sqkKeyWord
     * @param column
     * @param val
     * @return
     */
    private C addCondition(boolean condition, SqlKeywordEnum sqkKeyWord, R column, Object val) {
        if (condition) {
            String columnName = this.getColumnName(column);
            String namedColumnName = this.getNamedColumnName(columnName);

            this.putColumnValue(namedColumnName, val);
            String sqlSegment = String.format("%s %s %s", columnName, sqkKeyWord.getKeyword(), ":" + namedColumnName);
            segmentContainer.addAndSegment(sqlSegment);
        }

        return typeThis;
    }

    @Override
    public C eq(boolean condition, R column, Object val) {
        return addCondition(condition, SqlKeywordEnum.EQ, column, val);
    }

    @Override
    public C ne(boolean condition, R column, Object val) {
        return addCondition(condition, SqlKeywordEnum.NE, column, val);
    }

    @Override
    public C lt(boolean condition, R column, Object val) {
        return addCondition(condition, SqlKeywordEnum.LT, column, val);
    }

    @Override
    public C gt(boolean condition, R column, Object val) {
        return addCondition(condition, SqlKeywordEnum.GT, column, val);
    }

    @Override
    public C le(boolean condition, R column, Object val) {
        return addCondition(condition, SqlKeywordEnum.LT, column, val);
    }

    @Override
    public C ge(boolean condition, R column, Object val) {
        return addCondition(condition, SqlKeywordEnum.GE, column, val);
    }

    @Override
    public C between(boolean condition, R column, Object leftVal, Object rightVal) {
        if (condition) {
            String columnName = this.getColumnName(column);
            String namedColumnName = this.getNamedColumnName(columnName);
            String leftColumnName = namedColumnName + "#Left";
            String rightColumnName = namedColumnName + "#Right";
            this.putColumnValue(leftColumnName, leftVal);
            this.putColumnValue(rightColumnName, rightVal);
            String sqlSegment = String.format("%s %s %s %s %s",
                    columnName, SqlKeywordEnum.BETWEEN.getKeyword(), ":" + leftColumnName,
                    SqlKeywordEnum.AND.getKeyword(), ":" + rightColumnName);
            segmentContainer.addAndSegment(sqlSegment);
        }
        return typeThis;
    }

    public C addLikeCondition(boolean condition, R column, Object val, SqlLikeEnum sqlLikeEnum, boolean isLike) {
        if (condition) {
            String columnName = this.getColumnName(column);
            String namedColumnName = this.getNamedColumnName(columnName);
            this.putColumnValue(namedColumnName, sqlLikeEnum.getLikeVal(val));
            String sqlSegment = String.format("%s %s %s", columnName,
                    isLike ? SqlKeywordEnum.LIKE.getKeyword() : SqlKeywordEnum.NOT_LIKE.getKeyword(),
                    ":" + namedColumnName);
            segmentContainer.addAndSegment(sqlSegment);
        }
        return typeThis;
    }

    @Override
    public C like(boolean condition, R column, Object val) {
        return addLikeCondition(condition, column, val, SqlLikeEnum.LIKE, true);
    }

    @Override
    public C notLike(boolean condition, R column, Object val) {
        return addLikeCondition(condition, column, val, SqlLikeEnum.LIKE, false);
    }

    @Override
    public C likeLeft(boolean condition, R column, Object val) {
        return addLikeCondition(condition, column, val, SqlLikeEnum.LIKE_LEFT, true);
    }

    @Override
    public C likeRight(boolean condition, R column, Object val) {
        return addLikeCondition(condition, column, val, SqlLikeEnum.LIKE_RIGHT, true);
    }

    public C addNullCondition(boolean condition, R column, boolean isNull) {
        if (condition) {
            String columnName = this.getColumnName(column);
            String sqlSegment = String.format("%s %s", columnName,
                    isNull ? SqlKeywordEnum.IS_NULL.getKeyword() : SqlKeywordEnum.IS_NOT_NULL.getKeyword());
            segmentContainer.addAndSegment(sqlSegment);
        }
        return typeThis;
    }

    @Override
    public C isNull(boolean condition, R column) {
        return addNullCondition(condition, column, true);
    }

    @Override
    public C isNotNull(boolean condition, R column) {
        return addNullCondition(condition, column, false);
    }

    public C addInCondition(boolean condition, R column, Collection<?> coll, boolean isIn) {
        if (condition && CollUtils.isNotEmpty(coll)) {
            String columnName = this.getColumnName(column);
            String namedColumnName = this.getNamedColumnName(columnName);
            this.putColumnValue(namedColumnName, coll);
            String sqlSegment = String.format("%s %s %s", columnName,
                    isIn ? SqlKeywordEnum.IN.getKeyword() : SqlKeywordEnum.NOT_IN.getKeyword(), "( :" + namedColumnName + " )");
            segmentContainer.addAndSegment(sqlSegment);
        }
        return typeThis;
    }

    @Override
    public C in(boolean condition, R column, Collection<?> coll) {
        return addInCondition(condition, column, coll, true);
    }

    @Override
    public C notIn(boolean condition, R column, Collection<?> coll) {
        return addInCondition(condition, column, coll, false);
    }

    @SafeVarargs
    @Override
    public final C groupBy(boolean condition, R... columns) {
        if (condition) {
            for (R column : columns) {
                segmentContainer.addGroupBySegment(this.getColumnName(column));
            }
        }
        return typeThis;
    }

    @Override
    public C having(boolean condition, String havingSql, String... params) {
        if (condition) {
            segmentContainer.addHavingSegment(String.format(havingSql, params));
        }
        return typeThis;
    }

    @Override
    public C last(boolean condition, String lastSql) {
        this.lastSql = lastSql;
        return typeThis;
    }

    @SafeVarargs
    @Override
    public final C orderBy(boolean condition, boolean isAsc, R... columns) {
        if (condition) {
            Arrays.stream(columns)
                    .forEach(column -> segmentContainer.addOrderBySegment(
                            this.getColumnName(column)
                                    + " "
                                    + (isAsc ? SqlKeywordEnum.ASC.getKeyword() : SqlKeywordEnum.DESC.getKeyword())
                            )
                    );
        }
        return typeThis;
    }

    @Override
    public C apply(boolean condition, String sql) {
        if (condition) {
            segmentContainer.addPlainSegment(sql);
        }
        return typeThis;
    }

    @Override
    public C exists(boolean condition, String sql) {
        if (condition) {
            segmentContainer.addPlainSegment(SqlKeywordEnum.EXISTS.getKeyword() + "( " + sql +" )");
        }
        return typeThis;
    }

    @Override
    public C notExists(boolean condition, String sql) {
        if (condition) {
            segmentContainer.addPlainSegment(SqlKeywordEnum.NOT_EXISTS.getKeyword() + "( " + sql +" )");
        }
        return typeThis;
    }

}
