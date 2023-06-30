package com.hcc.repository.core.handler.insert;

import com.hcc.repository.annotation.IEnum;
import com.hcc.repository.annotation.IdType;
import com.hcc.repository.core.conditions.ICondition;
import com.hcc.repository.core.conditions.nativesql.NativeSqlCondition;
import com.hcc.repository.core.constants.SqlKeywordEnum;
import com.hcc.repository.core.constants.StrPool;
import com.hcc.repository.core.metadata.TableColumnInfo;
import com.hcc.repository.core.metadata.TableInfoHelper;
import com.hcc.repository.core.utils.Assert;
import com.hcc.repository.core.utils.CollUtils;
import com.hcc.repository.core.utils.ReflectUtils;
import com.hcc.repository.core.utils.StrUtils;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;

/**
 * InsertSpliceConditionHandler
 *
 * @author hushengjun
 * @date 2023/6/29
 */
@SuppressWarnings("unchecked")
public class BatchInsertSpliceHandler extends InsertHandler {

    @Override
    protected ICondition<?> prepareCondition() {
        Collection<?> firstArg = super.getFirstArg(Collection.class);
        Assert.isTrue(CollUtils.isNotEmpty(firstArg), "插入参数不能为空");

        NativeSqlCondition<?> condition = new NativeSqlCondition<>();
        String tableName = TableInfoHelper.getTableName(entityClass);

        // INSERT INTO table_name
        StringBuilder sql = new StringBuilder(SqlKeywordEnum.INSERT_INTO.getKeyword());
        sql.append(StrPool.SPACE).append(tableName);

        // 获取除id外的列名
        List<TableColumnInfo> columnInfos = TableInfoHelper.getColumnInfosWithOutIdColumn(entityClass);

        // INSERT INTO table_name(column1, column2, ...) VALUES
        List<String> insertColumnNames = columnInfos.stream().map(TableColumnInfo::getColumnName).collect(Collectors.toList());
        TableColumnInfo idColumnInfo = TableInfoHelper.getIdColumnInfo(entityClass);
        if (idColumnInfo != null && (IdType.GENERATE.equals(idColumnInfo.getIdType()) || IdType.SPECIFY.equals(idColumnInfo.getIdType()))) {
            insertColumnNames.add(idColumnInfo.getColumnName());
        }

        sql.append(StrPool.L_BRACKET)
                .append(StrUtils.join(StrPool.COMMA_SPACE, insertColumnNames))
                .append(StrPool.R_BRACKET)
                .append(StrPool.SPACE)
                .append(SqlKeywordEnum.VALUES.getKeyword())
                .append(StrPool.SPACE);

        List<String> allColumnNames = new ArrayList<>();
        int index = 0;
        for (Object entity : firstArg) {
            index++;
            List<String> columnNames = new ArrayList<>();
            for (TableColumnInfo c : columnInfos) {
                Object value = ReflectUtils.getValue(entity, c.getField());
                // 转换
                Object targetValue = value;
                if (c.needConvert()) {
                    targetValue = ReflectUtils.newInstanceForCache(c.getConverter()).convertToColumn(value);
                } else if (c.isAssignableFromIEnum()) {
                    targetValue = ((IEnum<?>) value).getValue();
                }
                // 自动填充处理
                if (c.needAutoFillInsert() && targetValue == null) {
                    targetValue = super.getInsertAutoFillValue(TableInfoHelper.getTableInfo(entityClass), c);
                }
                String columnNamePl = c.getColumnName() + StrPool.UNDERLINE + index;

                columnNames.add(StrPool.getPlaceholder(columnNamePl));
                condition.putParam(columnNamePl, targetValue);
            }

            // id单独处理一下
            if (idColumnInfo != null && (IdType.GENERATE.equals(idColumnInfo.getIdType()) || IdType.SPECIFY.equals(idColumnInfo.getIdType()))) {
                String columnNamePl = idColumnInfo.getColumnName() + StrPool.UNDERLINE + index;

                columnNames.add(StrPool.getPlaceholder(columnNamePl));
                condition.putParam(columnNamePl, super.getIdValue(idColumnInfo, entity));
            }

            allColumnNames.add(StrPool.L_BRACKET + StrUtils.join(StrPool.COMMA_SPACE, columnNames) + StrPool.R_BRACKET);
        }
        // INSERT INTO table_name(column1, column2, ...) VALUES (#{column1_1}, #{column1_2}, ...),(#{column1_2}, #{column1_2}, ...)
        sql.append(StrUtils.join(StrPool.COMMA_SPACE, allColumnNames));

        condition.sql(sql.toString());

        return condition;
    }

    @Override
    protected Object executeSql(String sql, Object[] args) {
        return jdbcOperations.update(sql, args);
    }

}
