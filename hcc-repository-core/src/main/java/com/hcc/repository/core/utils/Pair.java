package com.hcc.repository.core.utils;

/**
 * Pair
 *
 * @author hushengjun
 * @date 2023/3/22
 */
public class Pair<L, R> {

    private L left;
    private R right;

    private Pair(L l, R r) {
        this.left = l;
        this.right = r;
    }

    public static <L, R> Pair<L, R> of(L l, R r) {
        return new Pair<>(l, r);
    }

    public L getLeft() {
        return left;
    }

    public R getRight() {
        return right;
    }

}
