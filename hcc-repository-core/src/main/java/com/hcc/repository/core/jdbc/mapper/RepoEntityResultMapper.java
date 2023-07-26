package com.hcc.repository.core.jdbc.mapper;

import com.hcc.repository.annotation.IConverter;
import com.hcc.repository.core.convert.IEnumConverter;
import com.hcc.repository.core.jdbc.ResultMapper;
import com.hcc.repository.core.metadata.TableColumnInfo;
import com.hcc.repository.core.metadata.TableInfo;
import com.hcc.repository.core.metadata.TableInfoHelper;
import com.hcc.repository.core.utils.ConstructorUtils;
import com.hcc.repository.core.utils.JdbcUtils;
import com.hcc.repository.core.utils.ReflectUtils;

import java.lang.reflect.Field;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.util.Map;
import java.util.Optional;

/**
 * 隐射实体的ResultMapper转换器
 *
 * @author hushengjun
 * @date 2023/3/17
 */
public class RepoEntityResultMapper<T> implements ResultMapper<T> {

    private final Class<T> entityClass;

    public RepoEntityResultMapper(Class<T> entityClass) {
        this.entityClass = entityClass;
    }

    @Override
    @SuppressWarnings("unchecked")
    public T resultMap(ResultSet rs, int i) throws SQLException {
        T instance = ReflectUtils.newInstance(entityClass);

        TableInfo tableInfo = TableInfoHelper.getTableInfo(entityClass);
        Map<String, TableColumnInfo> columnNameColumnInfoMap = TableInfoHelper.getColumnNameColumnInfoMap(entityClass);
        Map<String, TableColumnInfo> fieldNameColumnInfoMap = TableInfoHelper.getFieldNameColumnInfoMap(entityClass);
        ResultSetMetaData rsMetaData = rs.getMetaData();

        int columnCount = rsMetaData.getColumnCount();
        for (int index = 1; index <= columnCount; index++) {
            String columnName = this.getColumnName(rsMetaData, index);
            // 对应实体元数据，先用列名获取，再用字段名获取
            TableColumnInfo tableColumnInfo = Optional.ofNullable(columnNameColumnInfoMap.get(columnName))
                    .orElse(fieldNameColumnInfoMap.get(columnName));
            if (tableColumnInfo == null) {
                continue;
            }
            Object columnValue = JdbcUtils.getResultSetValue(rs, index);
            if (columnValue == null) {
                continue;
            }

            Field field = tableColumnInfo.getField();
            Class<?> fieldTypeClass = field.getType();
            Object targetValue;

            Class<? extends IConverter> converter = null;
            if (tableColumnInfo.needConvert()) {
                // 用户自定义转换器
                converter = tableColumnInfo.getConverter();
            } else if (tableColumnInfo.isAssignableFromIEnum()) {
                // 枚举处理
                converter = IEnumConverter.class;
            }

            if (converter != null) {
                targetValue = this.newInstanceConverter(converter, tableColumnInfo.getField().getType()).convertToAttribute(columnValue);
            } else {
                targetValue = JdbcUtils.getResultSetValue(rs, index, fieldTypeClass);
            }

            // 执行监听器
            targetValue = ReflectUtils.newInstanceForCache(tableInfo.getPropSet())
                    .onPropSet(instance, targetValue, field.getName(), columnName);

            // 反射赋值
            ReflectUtils.setValue(instance, field, targetValue);
        }

        return instance;
    }

    /**
     * 实例化converter
     * @param converterClass
     * @param targetClass
     * @return
     */
    private IConverter newInstanceConverter(Class<? extends IConverter> converterClass, Class<?> targetClass) {
        return Optional.ofNullable(ReflectUtils.matchConstruct(converterClass, Class.class))
                .map(c -> (IConverter) ConstructorUtils.newInstance(c, targetClass))
                .orElseGet(() -> ReflectUtils.newInstance(converterClass));
    }

}
