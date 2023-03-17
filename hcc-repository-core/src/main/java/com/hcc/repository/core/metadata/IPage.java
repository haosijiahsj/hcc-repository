package com.hcc.repository.core.metadata;

import java.io.Serializable;
import java.util.List;
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 * IPage
 *
 * @author hushengjun
 * @date 2023/3/7
 */
public interface IPage<T> extends Serializable {

    long getCurrent();

    long getSize();

    long getPageCount();

    long getTotalRows();

    default long offset() {
        if (this.getCurrent() < 1L) {
            return 1L;
        }

        return (this.getCurrent() - 1L) * this.getSize();
    }

    boolean hasNext();

    List<T> getRecords();

    IPage<T> setRecords(List<T> records);

    default <R> IPage<R> convert(Function<? super T, ? extends R> function) {
        List<R> collect = this.getRecords().stream()
                .map(function)
                .collect(Collectors.toList());

        return ((IPage<R>) this).setRecords(collect);
    }

}
