package com.hcc.repository.core.handler.insert;

import com.hcc.repository.annotation.IdType;
import com.hcc.repository.core.conditions.ICondition;
import com.hcc.repository.core.conditions.nativesql.NativeSqlCondition;
import com.hcc.repository.core.constants.SqlKeywordEnum;
import com.hcc.repository.core.constants.StrPool;
import com.hcc.repository.core.jdbc.batch.MapPreparedStatementObjectSetter;
import com.hcc.repository.core.metadata.TableColumnInfo;
import com.hcc.repository.core.metadata.TableInfoHelper;
import com.hcc.repository.core.utils.Assert;
import com.hcc.repository.core.utils.CollUtils;
import com.hcc.repository.core.utils.Pair;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * BatchInsertHandler
 *
 * @author hushengjun
 * @date 2023/3/21
 */
public class BatchInsertHandler extends InsertHandler {

    private Map<Integer, String> indexColumnNameMap;

    @Override
    protected void prepare() {
        Collection<?> firstArg = super.getFirstArg(Collection.class);
        Assert.isTrue(CollUtils.isNotEmpty(firstArg), "插入参数不能为空");
        int batchInsertLimitSize = configuration.getBatchInsertLimitSize();
        Assert.isTrue(firstArg.size() <= batchInsertLimitSize, String.format("批量插入数量：%s，超出限制：%s", firstArg.size(), batchInsertLimitSize));
    }

    @Override
    protected ICondition<?> prepareCondition() {
        Collection<?> entities = super.getFirstArg(Collection.class);
        TableColumnInfo idColumnInfo = TableInfoHelper.getIdColumnInfo(entityClass);
        List<TableColumnInfo> columnInfos = TableInfoHelper.getColumnInfosWithOutIdColumn(entityClass);

        List<Map<String, Object>> paramMaps = new ArrayList<>(entities.size());
        for (Object entity : entities) {
            Map<String, Object> paramMap = new HashMap<>();
            // 处理id
            if (idColumnInfo != null && (IdType.GENERATE.equals(idColumnInfo.getIdType()) || IdType.SPECIFY.equals(idColumnInfo.getIdType()))) {
                // 这个方法会回填id到实体中
                paramMap.put(idColumnInfo.getColumnName(), super.getIdValue(idColumnInfo, entity));
            }

            // 处理列
            for (TableColumnInfo columnInfo : columnInfos) {
                Object columnValue = getColumnValue(columnInfo, entity);
                if (columnValue != null) {
                    paramMap.put(columnInfo.getColumnName(), columnValue);
                }
            }

            paramMaps.add(paramMap);
        }

        Pair<String, Map<Integer, String>> pair = this.buildInsertSql();

        NativeSqlCondition<?> condition = new NativeSqlCondition<>();
        condition.sql(pair.getLeft());
        condition.addArgs(new ArrayList<>(paramMaps));
        this.indexColumnNameMap = pair.getRight();

        return condition;
    }

    /**
     * 构建插入语句
     * @return
     */
    private Pair<String, Map<Integer, String>> buildInsertSql() {
        Map<Integer, String> indexColumnNameMap = new HashMap<>();
        List<String> sqlColumns = new ArrayList<>();

        int index = 0;

        // 获取除id外的列名
        List<TableColumnInfo> columnInfos = TableInfoHelper.getColumnInfosWithOutIdColumn(entityClass);
        for (TableColumnInfo columnInfo : columnInfos) {
            sqlColumns.add(columnInfo.getColumnName());
            indexColumnNameMap.put(++index, columnInfo.getColumnName());
        }

        TableColumnInfo idColumnInfo = TableInfoHelper.getIdColumnInfo(entityClass);
        if (idColumnInfo != null && (IdType.GENERATE.equals(idColumnInfo.getIdType()) || IdType.SPECIFY.equals(idColumnInfo.getIdType()))) {
            sqlColumns.add(idColumnInfo.getColumnName());
            indexColumnNameMap.put(++index, idColumnInfo.getColumnName());
        }

        String columnField = StrPool.L_BRACKET + String.join(StrPool.COMMA_SPACE, sqlColumns) + StrPool.R_BRACKET;
        String namedField = StrPool.L_BRACKET
                + sqlColumns.stream().map(c -> StrPool.QU_MASK).collect(Collectors.joining(StrPool.COMMA_SPACE))
                + StrPool.R_BRACKET;

        String sql =  String.join(StrPool.SPACE,
                SqlKeywordEnum.INSERT_INTO.getKeyword(),
                TableInfoHelper.getTableName(entityClass),
                columnField,
                SqlKeywordEnum.VALUES.getKeyword(),
                namedField
        );
        return Pair.of(sql, indexColumnNameMap);
    }

    @Override
    @SuppressWarnings("unchecked")
    protected Object executeSql(String sql, Object[] args) {
        List<Map<String, Object>> paramMaps = Arrays.stream(args).map(a -> (Map<String, Object>)a).collect(Collectors.toList());
        return jdbcOperations.batchUpdate(sql, paramMaps, new MapPreparedStatementObjectSetter(this.indexColumnNameMap));
    }

}
