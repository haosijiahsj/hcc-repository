package com.hcc.repository.core.page;

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

    long getCurPage();

    long getPageSize();

    default long getPageCount() {
        if (getPageSize() == 0L) {
            return 0L;
        }
        long pages = getTotalRows() / getPageSize();
        if (getTotalRows() % getPageSize() != 0) {
            pages++;
        }
        return pages;
    }

    long getTotalRows();

    default long offset() {
        if (this.getCurPage() < 1L) {
            return 1L;
        }

        return (this.getCurPage() - 1L) * this.getPageSize();
    }

    default boolean hasNext() {
        return getCurPage() < getPageCount();
    }

    List<T> getRecords();

    IPage<T> setRecords(List<T> records);

    IPage<T> setTotalRows(long totalRows);

    IPage<T> setCurPage(long curPage);

    IPage<T> setPageSize(long pageSize);

    @SuppressWarnings("unchecked")
    default <R> IPage<R> convert(Function<? super T, ? extends R> function) {
        List<R> collect = this.getRecords().stream()
                .map(function)
                .collect(Collectors.toList());

        return ((IPage<R>) this).setRecords(collect);
    }

}
