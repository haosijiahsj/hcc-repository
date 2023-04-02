package com.hcc.repository.core.handler.select;

import com.hcc.repository.core.conditions.ICondition;
import com.hcc.repository.core.conditions.query.AbstractQueryCondition;
import com.hcc.repository.core.conditions.query.DefaultQueryCondition;
import com.hcc.repository.core.constants.MethodNameEnum;
import com.hcc.repository.core.constants.ExecuteSqlTypeEnum;
import com.hcc.repository.core.handler.AbstractMethodHandler;

/**
 * AbstractCrudMethodHandler
 *
 * @author hushengjun
 * @date 2023/3/23
 */
public abstract class AbstractSelectHandler extends AbstractMethodHandler {

    @Override
    protected ICondition<?> assembleCondition() {
        ICondition<?> condition;
        if (firstArgIsNull()) {
            condition = new DefaultQueryCondition<>(entityClass);
        } else {
            condition = getFirstArg(ICondition.class);
        }
        if (!(condition instanceof AbstractQueryCondition)) {
            throw new UnsupportedOperationException("select仅支持使用Query的Condition");
        }

        // 设置具体需要获取sql的类型，默认是select的sql，当查询总数或delete语句时需要额外设置
        AbstractQueryCondition abstractQueryCondition = (AbstractQueryCondition) condition;
        if (MethodNameEnum.SELECT_COUNT.getMethodName().equals(getMethodName())) {
            abstractQueryCondition.setExecuteSqlType(ExecuteSqlTypeEnum.SELECT_COUNT);
        } else if (MethodNameEnum.DELETE.getMethodName().equals(getMethodName())) {
            abstractQueryCondition.setExecuteSqlType(ExecuteSqlTypeEnum.DELETE);
        } else {
            abstractQueryCondition.setExecuteSqlType(ExecuteSqlTypeEnum.SELECT);
        }

        return condition;
    }

}
