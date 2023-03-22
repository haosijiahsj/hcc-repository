package com.hcc.repository.core.conditions.query;

import com.hcc.repository.core.conditions.AbstractLambdaCondition;
import com.hcc.repository.core.conditions.interfaces.SFunction;
import com.hcc.repository.core.conditions.interfaces.SelectClause;
import com.hcc.repository.core.metadata.TableInfo;
import com.hcc.repository.core.metadata.TableInfoHelper;
import com.hcc.repository.core.utils.StrUtils;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

/**
 * LambdaQueryConditions
 *
 * @author hushengjun
 * @date 2023/3/3
 */
public class LambdaQueryCondition<T> extends AbstractLambdaCondition<T, LambdaQueryCondition<T>> implements SelectClause<LambdaQueryCondition<T>, T, SFunction<T, ?>> {

    private final List<String> selectColumns;

    public LambdaQueryCondition() {
        this((T) null);
//        Type genericSuperclass = this.getClass().getGenericSuperclass();
//        ParameterizedType parameterizedType = (ParameterizedType) genericSuperclass;
//        Type[] actualTypeArguments = parameterizedType.getActualTypeArguments();
//        Type actualTypeArgument = actualTypeArguments[0];
//        setEntityClass((Class<T>) actualTypeArgument);
    }

    public LambdaQueryCondition(T entity) {
        super.init();
        selectColumns = new ArrayList<>(32);
        setEntity(entity);
    }

    public LambdaQueryCondition(Class<T> entityClass) {
        super.init();
        selectColumns = new ArrayList<>(32);
        setEntityClass(entityClass);
    }

    @SafeVarargs
    @Override
    public final LambdaQueryCondition<T> select(SFunction<T, ?>... columns) {
        selectColumns.addAll(
                Arrays.stream(columns)
                        .map(this::getColumnName)
                        .collect(Collectors.toList())
        );
        return typeThis;
    }

    @Override
    public String getSqlSelect() {
        return "SELECT " + (selectColumns.isEmpty() ? "*" : String.join(", ", selectColumns));
    }

    @Override
    public String getSqlWhere() {
        String lastSql = getLastSql();

        return getSegmentContainer().getSqlSegment()
                + (StrUtils.isEmpty(lastSql) ? "" : " " + lastSql);
    }

}
