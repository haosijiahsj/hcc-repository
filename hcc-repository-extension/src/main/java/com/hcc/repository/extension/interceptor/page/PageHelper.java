package com.hcc.repository.extension.interceptor.page;

import com.hcc.repository.core.page.DefaultPage;
import com.hcc.repository.core.page.IPage;

/**
 * PageHelper
 *
 * @author hushengjun
 * @date 2023/4/8
 */
public class PageHelper {

    private static final ThreadLocal<IPage> HOLDER = new ThreadLocal<>();

    public static void startPage(long curPage, long pageSize) {
        IPage page = new DefaultPage();
        page.setCurPage(curPage);
        page.setPageSize(pageSize);

        HOLDER.set(page);
    }

    public static IPage getPageResult() {
        return HOLDER.get();
    }

}
