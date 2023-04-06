package com.hcc.repository.core.conditions.interfaces;

import java.util.Arrays;
import java.util.Collection;
import java.util.Optional;
import java.util.stream.Collectors;

/**
 * WhereClause
 *
 * @author hushengjun
 * @date 2023/3/14
 */
public interface WhereClause<C, R> {

    C eq(boolean condition, R column, Object val);

    default C eq(R column, Object val) {
        return eq(true, column, val);
    }

    C ne(boolean condition, R column, Object val);

    default C ne(R column, Object val) {
        return ne(true, column, val);
    }

    C lt(boolean condition, R column, Object val);

    default C lt(R column, Object val) {
        return lt(true, column, val);
    }

    C gt(boolean condition, R column, Object val);

    default C gt(R column, Object val) {
        return gt(true, column, val);
    }

    C le(boolean condition, R column, Object val);

    default C le(R column, Object val) {
        return le(true, column, val);
    }

    C ge(boolean condition, R column, Object val);

    default C ge(R column, Object val) {
        return ge(true, column, val);
    }

    C between(boolean condition, R column, Object leftVal, Object rightVal);

    default C between(R column, Object leftVal, Object rightVal) {
        return between(true, column, leftVal, rightVal);
    }

    C notBetween(boolean condition, R column, Object leftVal, Object rightVal);

    default C notBetween(R column, Object leftVal, Object rightVal) {
        return notBetween(true, column, leftVal, rightVal);
    }

    C like(boolean condition, R column, Object val);

    default C like(R column, Object val) {
        return like(true, column, val);
    }

    C notLike(boolean condition, R column, Object val);

    default C notLike(R column, Object val) {
        return notLike(true, column, val);
    }

    C likeLeft(boolean condition, R column, Object val);

    default C likeLeft(R column, Object val) {
        return likeLeft(true, column, val);
    }

    C likeRight(boolean condition, R column, Object val);

    default C likeRight(R column, Object val) {
        return likeRight(true, column, val);
    }

    C isNull(boolean condition, R column);

    default C isNull(R column) {
        return isNull(true, column);
    }

    C isNotNull(boolean condition, R column);

    default C isNotNull(R column) {
        return isNotNull(true, column);
    }

    C in(boolean condition, R column, Collection<?> coll);

    default C in(R column, Collection<?> coll) {
        return in(true, column, coll);
    }

    C notIn(boolean condition, R column, Collection<?> coll);

    default C notIn(R column, Collection<?> coll) {
        return in(true, column, coll);
    }

    default C in(boolean condition, R column, Object...args) {
        return in(condition, column, Arrays.stream(Optional.ofNullable(args).orElseGet(() -> new Object[] {})).collect(Collectors.toList()));
    }

    default C in(R column, Object...args) {
        return in(true, column, args);
    }

    default C notIn(boolean condition, R column, Object...args) {
        return notIn(condition, column, Arrays.stream(Optional.ofNullable(args).orElseGet(() -> new Object[] {})).collect(Collectors.toList()));
    }

    default C notIn(R column, Object...args) {
        return notIn(true, column, args);
    }

    default C inSql(R column, String value) {
        return inSql(true, column, value);
    }

    C inSql(boolean condition, R column, String value);

    default C notInSql(R column, String value) {
        return notInSql(true, column, value);
    }

    C notInSql(boolean condition, R column, String value);

}
