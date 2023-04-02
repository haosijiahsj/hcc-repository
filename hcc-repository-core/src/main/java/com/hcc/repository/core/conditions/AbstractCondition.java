package com.hcc.repository.core.conditions;

import com.hcc.repository.core.conditions.interfaces.ExtWhereClause;
import com.hcc.repository.core.conditions.interfaces.GroupByClause;
import com.hcc.repository.core.conditions.interfaces.NestedClause;
import com.hcc.repository.core.conditions.interfaces.OrderByClause;
import com.hcc.repository.core.conditions.interfaces.WhereClause;
import com.hcc.repository.core.constants.SqlKeywordEnum;
import com.hcc.repository.core.constants.SqlLikeEnum;
import com.hcc.repository.core.constants.StrPool;
import com.hcc.repository.core.utils.CollUtils;
import com.hcc.repository.core.utils.StrUtils;

import java.util.Arrays;
import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Consumer;

/**
 * AbstractCondition
 *
 * @author hushengjun
 * @date 2023/3/10
 */
@SuppressWarnings("unchecked")
public abstract class AbstractCondition<T, R, C extends AbstractCondition<T, R, C>> extends ICondition<T>
        implements WhereClause<C, R>, GroupByClause<C, R>, OrderByClause<C, R>, ExtWhereClause<C, R>, NestedClause<C, C> {

    protected AtomicInteger pos;
    protected C typeThis = (C) this;

    private T entity;
    protected Class<T> entityClass;
    protected Map<String, Object> columnValuePairs;
    protected SegmentContainer segmentContainer;

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

    public C putColumnValue(String column, Object val) {
        columnValuePairs.put(column, val);
        return typeThis;
    }

    @Override
    public Map<String, Object> getColumnValuePairs() {
        return columnValuePairs;
    }

    public SegmentContainer getSegmentContainer() {
        return segmentContainer;
    }

    protected String getColumnName(R column) {
        if (column instanceof String) {
            return (String) column;
        }
        throw new IllegalArgumentException("需要实现获取列名方法");
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
            String sqlSegment = String.format("%s %s %s", columnName, sqkKeyWord.getKeyword(), StrPool.COLON + namedColumnName);
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
        return addCondition(condition, SqlKeywordEnum.LE, column, val);
    }

    @Override
    public C ge(boolean condition, R column, Object val) {
        return addCondition(condition, SqlKeywordEnum.GE, column, val);
    }

    private C addBetweenCondition(boolean condition, R column, Object leftVal, Object rightVal, boolean isBetween) {
        if (condition) {
            String columnName = this.getColumnName(column);
            String namedColumnName = this.getNamedColumnName(columnName);
            String leftColumnName = namedColumnName + "#Left";
            String rightColumnName = namedColumnName + "#Right";
            this.putColumnValue(leftColumnName, leftVal)
                    .putColumnValue(rightColumnName, rightVal);
            SqlKeywordEnum sqlKeyword = isBetween ? SqlKeywordEnum.BETWEEN : SqlKeywordEnum.NOT_BETWEEN;
            String sqlSegment = String.format("%s %s %s %s %s",
                    columnName, sqlKeyword.getKeyword(), StrPool.COLON + leftColumnName,
                    SqlKeywordEnum.AND.getKeyword(), StrPool.COLON + rightColumnName);
            segmentContainer.addAndSegment(sqlSegment);
        }
        return typeThis;
    }

    @Override
    public C between(boolean condition, R column, Object leftVal, Object rightVal) {
        return addBetweenCondition(condition, column, leftVal, rightVal, true);
    }

    @Override
    public C notBetween(boolean condition, R column, Object leftVal, Object rightVal) {
        return addBetweenCondition(condition, column, leftVal, rightVal, false);
    }

    private C addLikeCondition(boolean condition, R column, Object val, SqlLikeEnum sqlLikeEnum, boolean isLike) {
        if (condition) {
            String columnName = this.getColumnName(column);
            String namedColumnName = this.getNamedColumnName(columnName);
            this.putColumnValue(namedColumnName, sqlLikeEnum.getLikeVal(val));
            String sqlSegment = String.format("%s %s %s", columnName,
                    isLike ? SqlKeywordEnum.LIKE.getKeyword() : SqlKeywordEnum.NOT_LIKE.getKeyword(),
                    StrPool.COLON + namedColumnName);
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

    private C addNullCondition(boolean condition, R column, boolean isNull) {
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

    private C addInCondition(boolean condition, R column, Collection<?> coll, boolean isIn) {
        if (condition && CollUtils.isNotEmpty(coll)) {
            String columnName = this.getColumnName(column);
            String namedColumnName = this.getNamedColumnName(columnName);
            this.putColumnValue(namedColumnName, coll);
            String sqlSegment = String.format("%s %s %s", columnName,
                    isIn ? SqlKeywordEnum.IN.getKeyword() : SqlKeywordEnum.NOT_IN.getKeyword(), StrPool.L_BRACKET + StrPool.COLON + namedColumnName + StrPool.R_BRACKET);
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
    public C having(boolean condition, String havingSql) {
        if (condition) {
            segmentContainer.addHavingSegment(havingSql);
        }
        return typeThis;
    }

    @Override
    public C last(boolean condition, String lastSql) {
//        this.lastSql = lastSql;
        if (condition) {
            segmentContainer.setLastSql(lastSql);
        }
        return typeThis;
    }

    @SafeVarargs
    @Override
    public final C orderBy(boolean condition, boolean isAsc, R... columns) {
        if (condition) {
            Arrays.stream(columns)
                    .forEach(column -> segmentContainer.addOrderBySegment(
                                    this.getColumnName(column)
                                            + StrPool.SPACE
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
            segmentContainer.addPlainSegment(SqlKeywordEnum.EXISTS.getKeyword() + StrPool.L_BRACKET + sql + StrPool.R_BRACKET);
        }
        return typeThis;
    }

    @Override
    public C notExists(boolean condition, String sql) {
        if (condition) {
            segmentContainer.addPlainSegment(SqlKeywordEnum.NOT_EXISTS.getKeyword() + StrPool.L_BRACKET + sql + StrPool.R_BRACKET);
        }
        return typeThis;
    }

    private C addNestedCondition(boolean condition, Consumer<C> consumer, SqlKeywordEnum sqlKeyword) {
        if (condition) {
            C newC = this.newInstance();
            consumer.accept(newC);
            String sqlKeywordStr = sqlKeyword == null ? StrPool.EMPTY : sqlKeyword.getKeyword();
            String sqlSegment = newC.getSegmentContainer().getSqlSegmentAfterWhere();
            if (StrUtils.isNotEmpty(sqlSegment)) {
                sqlSegment = sqlSegment.replaceAll(SqlKeywordEnum.WHERE.getKeyword(), StrPool.EMPTY).trim();
                String sqlNested = sqlKeywordStr + StrPool.SPACE + StrPool.L_BRACKET + sqlSegment + StrPool.R_BRACKET;
                segmentContainer.addPlainSegment(sqlNested);
            }
        }
        return typeThis;
    }

    protected C newInstance() {
        return null;
    }

    @Override
    public C and(boolean condition, Consumer<C> consumer) {
        return addNestedCondition(condition, consumer, SqlKeywordEnum.AND);
    }

    @Override
    public C and() {
        segmentContainer.addPlainSegment(SqlKeywordEnum.AND.getKeyword());
        return typeThis;
    }

    @Override
    public C or(boolean condition, Consumer<C> consumer) {
        return addNestedCondition(condition, consumer, SqlKeywordEnum.OR);
    }

    @Override
    public C or() {
        segmentContainer.addPlainSegment(SqlKeywordEnum.OR.getKeyword());
        return typeThis;
    }

    @Override
    public C nested(boolean condition, Consumer<C> consumer) {
        return addNestedCondition(condition, consumer, null);
    }

}
