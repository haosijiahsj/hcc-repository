package com.hcc.repository.extension.conditions;

import com.hcc.repository.core.conditions.AbstractCondition;
import com.hcc.repository.core.conditions.ICondition;
import com.hcc.repository.core.conditions.interfaces.ExtWhereClause;
import com.hcc.repository.core.conditions.interfaces.GroupByClause;
import com.hcc.repository.core.conditions.interfaces.NestedClause;
import com.hcc.repository.core.conditions.interfaces.OrderByClause;
import com.hcc.repository.core.conditions.interfaces.WhereClause;
import com.hcc.repository.core.constants.SqlKeywordEnum;
import com.hcc.repository.core.constants.StrPool;

import java.util.Collection;
import java.util.Map;
import java.util.function.Consumer;

/**
 * 这里重新实现一遍，麻烦
 *
 * @author hushengjun
 * @date 2023/4/4
 */
@SuppressWarnings("unchecked")
public abstract class AbstractChainCondition<T, R, C extends AbstractChainCondition<T, R, C, CH>, CH extends AbstractCondition<T, R, CH>>
        extends ICondition<T> implements WhereClause<C, R>, GroupByClause<C, R>, OrderByClause<C, R>, ExtWhereClause<C, R>, NestedClause<CH, C> {

    protected C typeThis = (C) this;
    protected CH ch;

    public AbstractCondition<T, R, CH> getCondition() {
        return ch;
    }

    @Override
    public T getEntity() {
        return ch.getEntity();
    }

    @Override
    public Class<?> getEntityClass() {
        return ch.getEntityClass();
    }

    @Override
    public String getExecuteSql() {
        return ch.getExecuteSql();
    }

    @Override
    public Map<String, Object> getColumnValuePairs() {
        return ch.getColumnValuePairs();
    }

    @Override
    public C last(boolean condition, String lastSql) {
        getCondition().last(condition, lastSql);
        return typeThis;
    }

    @Override
    public C apply(boolean condition, String sql) {
        getCondition().apply(condition, sql);
        return typeThis;
    }

    @Override
    public C exists(boolean condition, String sql) {
        getCondition().exists(condition, sql);
        return typeThis;
    }

    @Override
    public C notExists(boolean condition, String sql) {
        getCondition().notExists(condition, sql);
        return typeThis;
    }

    @SafeVarargs
    @Override
    public final C groupBy(boolean condition, R... columns) {
        getCondition().groupBy(condition, columns);
        return typeThis;
    }

    @Override
    public C having(boolean condition, String havingSql) {
        getCondition().having(condition, havingSql);
        return typeThis;
    }

    @Override
    public C and(boolean condition, Consumer<CH> consumer) {
        getCondition().and(condition, consumer);
        return typeThis;
    }

    @Override
    public C and() {
        getCondition().and();
        return typeThis;
    }

    @Override
    public C or(boolean condition, Consumer<CH> consumer) {
        getCondition().or(condition, consumer);
        return typeThis;
    }

    @Override
    public C or() {
        getCondition().or();
        return typeThis;
    }

    @Override
    public C nested(boolean condition, Consumer<CH> consumer) {
        getCondition().nested(condition, consumer);
        return typeThis;
    }

    @SafeVarargs
    @Override
    public final C orderBy(boolean condition, boolean isAsc, R... columns) {
        getCondition().orderBy(condition, isAsc, columns);
        return typeThis;
    }

    @Override
    public C eq(boolean condition, R column, Object val) {
        getCondition().eq(condition, column, val);
        return typeThis;
    }

    @Override
    public C ne(boolean condition, R column, Object val) {
        getCondition().eq(condition, column, val);
        return typeThis;
    }

    @Override
    public C lt(boolean condition, R column, Object val) {
        getCondition().eq(condition, column, val);
        return typeThis;
    }

    @Override
    public C gt(boolean condition, R column, Object val) {
        getCondition().gt(condition, column, val);
        return typeThis;
    }

    @Override
    public C le(boolean condition, R column, Object val) {
        getCondition().le(condition, column, val);
        return typeThis;
    }

    @Override
    public C ge(boolean condition, R column, Object val) {
        getCondition().ge(condition, column, val);
        return typeThis;
    }

    @Override
    public C between(boolean condition, R column, Object leftVal, Object rightVal) {
        getCondition().between(condition, column, leftVal, rightVal);
        return typeThis;
    }

    @Override
    public C notBetween(boolean condition, R column, Object leftVal, Object rightVal) {
        getCondition().notBetween(condition, column, leftVal, rightVal);
        return typeThis;
    }

    @Override
    public C like(boolean condition, R column, Object val) {
        getCondition().like(condition, column, val);
        return typeThis;
    }

    @Override
    public C notLike(boolean condition, R column, Object val) {
        getCondition().notLike(condition, column, val);
        return typeThis;
    }

    @Override
    public C likeLeft(boolean condition, R column, Object val) {
        getCondition().likeLeft(condition, column, val);
        return typeThis;
    }

    @Override
    public C likeRight(boolean condition, R column, Object val) {
        getCondition().likeRight(condition, column, val);
        return typeThis;
    }

    @Override
    public C isNull(boolean condition, R column) {
        getCondition().isNull(condition, column);
        return typeThis;
    }

    @Override
    public C isNotNull(boolean condition, R column) {
        getCondition().isNotNull(condition, column);
        return typeThis;
    }

    @Override
    public C in(boolean condition, R column, Collection<?> coll) {
        getCondition().in(condition, column, coll);
        return typeThis;
    }

    @Override
    public C notIn(boolean condition, R column, Collection<?> coll) {
        getCondition().notIn(condition, column, coll);
        return typeThis;
    }

    @Override
    public C inSql(boolean condition, R column, String value) {
        getCondition().inSql(condition, column, value);
        return typeThis;
    }

    @Override
    public C notInSql(boolean condition, R column, String value) {
        getCondition().notInSql(condition, column, value);
        return typeThis;
    }

}
