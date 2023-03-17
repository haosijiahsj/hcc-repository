package com.hcc.repository.core.condition.query;

import com.hcc.repository.core.condition.AbstractLambdaCondition;
import com.hcc.repository.core.condition.interfaces.SFunction;
import com.hcc.repository.core.condition.interfaces.SelectClause;

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
        super.init();
        selectColumns = new ArrayList<>(32);
    }

    @Override
    public LambdaQueryCondition<T> select(SFunction<T, ?>... columns) {
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

}
