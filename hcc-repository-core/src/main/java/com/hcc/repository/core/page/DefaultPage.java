package com.hcc.repository.core.page;

import java.util.ArrayList;
import java.util.List;

/**
 * DefaultPage
 *
 * @author hushengjun
 * @date 2023/3/25
 */
public class DefaultPage<T> implements IPage<T> {

    private long curPage = 1L;
    private long pageSize = 20L;
    private long totalRows;
    private List<T> records = new ArrayList<>();

    public DefaultPage() {}

    public DefaultPage(long curPage, long pageSize) {
        this.curPage = curPage;
        this.pageSize = pageSize;
    }

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
    public List<T> getRecords() {
        return records;
    }

    @Override
    public IPage<T> setRecords(List<T> records) {
        if (records != null) {
            this.records = records;
        }
        return this;
    }

    @Override
    public IPage<T> setTotalRows(long totalRows) {
        this.totalRows = totalRows;
        return this;
    }

    @Override
    public IPage<T> setCurPage(long curPage) {
        this.curPage = curPage;
        return this;
    }

    @Override
    public IPage<T> setPageSize(long pageSize) {
        this.pageSize = pageSize;
        return this;
    }

}
