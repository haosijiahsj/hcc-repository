package com.hcc.repository.core.conditions.insert;

import com.hcc.repository.core.conditions.ICondition;
import com.hcc.repository.core.metadata.TableInfoHelper;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * AbstractInsertCondition
 *
 * @author hushengjun
 * @date 2023/3/22
 */
public abstract class AbstractInsertCondition<T, R> extends ICondition<T> {

    private T entity;
    private Class<T> entityClass;
    protected Map<String, Object> columnValuePairs;

    protected List<String> sqlColumns;
    protected List<String> sqlValues;

    protected void init(T entity) {
        this.entity = entity;
        this.columnValuePairs = new HashMap<>(32);
        this.sqlColumns = new ArrayList<>(32);
        this.sqlValues = new ArrayList<>(32);
    }

    @Override
    public T getEntity() {
        return entity;
    }

    @Override
    public Class<?> getEntityClass() {
        return entity == null ? entityClass : entity.getClass();
    }

    @Override
    public Map<String, Object> getColumnValuePairs() {
        return columnValuePairs;
    }

    public AbstractInsertCondition<T, R> value(boolean condition, R column, Object val) {
        if (condition) {
            String columnName = (String) column;
            sqlColumns.add(columnName);
            sqlValues.add(columnName);
            columnValuePairs.put(columnName, val);
        }

        return this;
    }

    public AbstractInsertCondition<T, R> value(R column, Object val) {
        return value(true, column, val);
    }

    @Override
    public String getSqlInsert() {
        String columnField = "(" + String.join(", ", sqlColumns) + ")";
        String namedField = "(" + sqlValues.stream().map(c -> ":" + c).collect(Collectors.joining(", ")) + ")";

        StringBuilder sb = new StringBuilder();
        sb.append("INSERT INTO ")
                .append(TableInfoHelper.getTableName(getEntityClass()))
                .append(" ")
                .append(columnField)
                .append(" VALUES ")
                .append(namedField);

        return sb.toString();
    }

    @Override
    public String getSqlCount() {
        throw new UnsupportedOperationException();
    }

    @Override
    public String getSqlDelete() {
        throw new UnsupportedOperationException();
    }

    @Override
    public String getSqlUpdate() {
        throw new UnsupportedOperationException();
    }

    @Override
    public String getSqlQuery() {
        throw new UnsupportedOperationException();
    }

}
