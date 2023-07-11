package com.hcc.repository.core.page;

import java.util.List;

/**
 * DefaultPageParam
 *
 * @author hushengjun
 * @date 2023/7/11
 */
public class DefaultPageParam implements IPage<Object> {
    private long curPage = 1L;
    private long pageSize = 20L;
    private long totalRows = 0L;

    @Override
    public long getCurPage() {
        return curPage;
    }

    @Override
    public long getPageSize() {
        return pageSize;
    }

    @Override
    public long getTotalRows() {
        return totalRows;
    }

    @Override
    public List<Object> getRecords() {
        throw new UnsupportedOperationException();
    }

    @Override
    public IPage<Object> setRecords(List<Object> records) {
        throw new UnsupportedOperationException();
    }

    @Override
    public IPage<Object> setTotalRows(long totalRows) {
        this.totalRows = totalRows;
        return this;
    }

    @Override
    public IPage<Object> setCurPage(long curPage) {
        this.curPage = curPage;
        return this;
    }

    @Override
    public IPage<Object> setPageSize(long pageSize) {
        this.pageSize = pageSize;
        return this;
    }

}
