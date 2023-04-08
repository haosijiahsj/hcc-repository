package com.hcc.repository.core.handler.select;

import com.hcc.repository.core.conditions.ICondition;
import com.hcc.repository.core.conditions.query.AbstractQueryCondition;
import com.hcc.repository.core.conditions.query.DefaultQueryCondition;
import com.hcc.repository.core.conditions.query.LambdaQueryCondition;
import com.hcc.repository.core.constants.ExecuteSqlTypeEnum;
import com.hcc.repository.core.handler.AbstractMethodHandler;
import com.hcc.repository.core.page.DefaultPage;
import com.hcc.repository.core.page.IPage;

import java.util.List;

/**
 * selectPageHandler
 *
 * @author hushengjun
 * @date 2023/3/26
 */
@Deprecated
public class SelectPageHandler extends AbstractMethodHandler {

    @Override
    protected ICondition<?> prepareCondition() {
        return null;
    }

    @Override
    protected Object executeSql(String sql, Object[] args) {
        return null;
    }

    @Override
    public Object handle() throws Exception {
        ICondition<?> condition = getFirstArg(ICondition.class);
        if (condition == null) {
            condition = new DefaultQueryCondition<>();
        }
        condition.setEntityClass(entityClass);

        ((AbstractQueryCondition) condition).setExecuteSqlType(ExecuteSqlTypeEnum.SELECT_COUNT);

        // 查询总数
        Long count;
        try {
           count = jdbcTemplateProxy.namedQueryForObject(condition.getExecuteSql(), condition.getColumnValuePairs(), Long.class);
        } catch (Exception e) {
            count = 0L;
        }

        // 分页参数
        IPage<?> pageParam = getPageParam(args);

        // 分页结果
        IPage<?> pageResult = new DefaultPage<>()
                .setCurPage(pageParam.getCurPage())
                .setPageSize(pageParam.getPageSize())
                .setTotalRows(count);
        if (count == 0L) {
            return pageResult;
        }

        // 添加LIMIT参数，默认mysql实现
        if (condition instanceof DefaultQueryCondition) {
            ((DefaultQueryCondition<?>) condition).last("LIMIT :offset_for_selectPage, :size_for_selectPage")
                    .putColumnValue("offset_for_selectPage", pageParam.offset())
                    .putColumnValue("size_for_selectPage", pageParam.getPageSize());
        } else if (condition instanceof LambdaQueryCondition) {
            ((LambdaQueryCondition<?>) condition).last("LIMIT :offset_for_selectPage, :size_for_selectPage")
                    .putColumnValue("offset_for_selectPage", pageParam.offset())
                    .putColumnValue("size_for_selectPage", pageParam.getPageSize());
        }

        ((AbstractQueryCondition) condition).setExecuteSqlType(ExecuteSqlTypeEnum.SELECT);

        // 查询数据
        List results = jdbcTemplateProxy.namedQueryForEntityList(condition.getExecuteSql(), condition.getColumnValuePairs(), entityClass);
        pageResult.setRecords(results);

        return pageResult;
    }

    private IPage<?> getPageParam(Object[] args) {
        if (args == null) {
            return new DefaultPage<>();
        }
        IPage<?> page = new DefaultPage<>();
        for (Object arg : args) {
            if (arg instanceof IPage) {
                page = (IPage<?>) arg;
            }
        }

        return page;
    }

}
