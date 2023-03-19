package com.hcc.repository.core.condition;

import com.hcc.repository.core.condition.interfaces.ExtWhereClause;
import com.hcc.repository.core.condition.interfaces.GroupByClause;
import com.hcc.repository.core.condition.interfaces.OrderByClause;
import com.hcc.repository.core.condition.interfaces.WhereClause;
import com.hcc.repository.core.constants.SqlKeywordEnum;
import com.hcc.repository.core.utils.CollUtils;

import java.util.Arrays;
import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;

/**
 * AbstractCondition
 *
 * @author hushengjun
 * @date 2023/3/10
 */
public abstract class AbstractCondition<T, R, C extends AbstractCondition<T, R, C>> extends ICondition<T>
        implements WhereClause<C, R>, GroupByClause<C, R>, OrderByClause<C, R>, ExtWhereClause<C, R> {

    private AtomicInteger pos;
    protected C typeThis = (C) this;

    private T entity;
    private Class<T> entityClass;
    private String firstSql;
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

    public void setEntityClass(Class<T> entityClass) {
        this.entityClass = entityClass;
    }

    public Class<T> getEntityClass() {
        return entity == null ? entityClass : (Class<T>) entity.getClass();
    }

    public void putColumnValue(String column, Object val) {
        columnValuePairs.put(column, val);
    }

    protected String getColumnName(R column) {
        return (String) column;
    }

    private String getNamedColumnName(String originalColumnName) {
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
            String leftColumnName = columnName + "#Left";
            String rightColumnName = columnName + "#Right";
            this.putColumnValue(leftColumnName, leftVal);
            this.putColumnValue(rightColumnName, rightVal);
            String sqlSegment = String.format("%s %s %s %s %s",
                    columnName, SqlKeywordEnum.BETWEEN.getKeyword(), ":" + leftColumnName,
                    SqlKeywordEnum.AND.getKeyword(), ":" + rightColumnName);
            segmentContainer.addAndSegment(sqlSegment);
        }
        return typeThis;
    }

    @Override
    public C like(boolean condition, R column, Object val) {
        if (condition) {
            String columnName = this.getColumnName(column);
            this.putColumnValue(columnName, val);
            String sqlSegment = String.format("%s %s %s", columnName, SqlKeywordEnum.LIKE.getKeyword(), "%:" + columnName + "%");
            segmentContainer.addAndSegment(sqlSegment);
        }
        return typeThis;
    }

    @Override
    public C notLike(boolean condition, R column, Object val) {
        if (condition) {
            String columnName = this.getColumnName(column);
            this.putColumnValue(columnName, val);
            String sqlSegment = String.format("%s %s %s", columnName, SqlKeywordEnum.NOT_LIKE.getKeyword(), "%:" + columnName + "%");
            segmentContainer.addAndSegment(sqlSegment);
        }
        return typeThis;
    }

    @Override
    public C likeLeft(boolean condition, R column, Object val) {
        if (condition) {
            String columnName = this.getColumnName(column);
            this.putColumnValue(columnName, val);
            String sqlSegment = String.format("%s %s %s", columnName, SqlKeywordEnum.LIKE.getKeyword(), ":" + columnName + "%");
            segmentContainer.addAndSegment(sqlSegment);
        }
        return typeThis;
    }

    @Override
    public C likeRight(boolean condition, R column, Object val) {
        if (condition) {
            String columnName = this.getColumnName(column);
            this.putColumnValue(columnName, val);
            String sqlSegment = String.format("%s %s %s", columnName, SqlKeywordEnum.LIKE.getKeyword(), ":%" + columnName);
            segmentContainer.addAndSegment(sqlSegment);
        }
        return typeThis;
    }

    @Override
    public C isNull(boolean condition, R column) {
        if (condition) {
            String columnName = this.getColumnName(column);
            String sqlSegment = String.format("%s %s", columnName, SqlKeywordEnum.IS_NULL.getKeyword());
            segmentContainer.addAndSegment(sqlSegment);
        }
        return typeThis;
    }

    @Override
    public C isNotNull(boolean condition, R column) {
        if (condition) {
            String columnName = this.getColumnName(column);
            String sqlSegment = String.format("%s %s", columnName, SqlKeywordEnum.IS_NOT_NULL.getKeyword());
            segmentContainer.addAndSegment(sqlSegment);
        }
        return typeThis;
    }

    @Override
    public C in(boolean condition, R column, Collection<?> coll) {
        if (condition && CollUtils.isNotEmpty(coll)) {
            String columnName = this.getColumnName(column);
            String sqlSegment = String.format("%s %s %s", columnName, SqlKeywordEnum.IN.getKeyword(), "(:" + columnName + ")");
            segmentContainer.addAndSegment(sqlSegment);
        }
        return typeThis;
    }

    @Override
    public C notIn(boolean condition, R column, Collection<?> coll) {
        if (condition && CollUtils.isNotEmpty(coll)) {
            String columnName = this.getColumnName(column);
            String sqlSegment = String.format("%s %s %s", columnName, SqlKeywordEnum.NOT_IN.getKeyword(), "(:" + columnName + ")");
            segmentContainer.addAndSegment(sqlSegment);
        }
        return typeThis;
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
            for (R column : columns) {
                String columnName = this.getColumnName(column);
                if (isAsc) {
                    segmentContainer.addOrderBySegment(columnName + " " + SqlKeywordEnum.ASC.getKeyword());
                } else {
                    segmentContainer.addOrderBySegment(columnName + " " + SqlKeywordEnum.DESC.getKeyword());
                }
            }
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