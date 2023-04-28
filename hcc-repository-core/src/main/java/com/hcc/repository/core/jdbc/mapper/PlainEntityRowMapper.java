package com.hcc.repository.core.jdbc.mapper;

import com.hcc.repository.core.convert.ConverterFactory;
import com.hcc.repository.core.convert.ValueConverter;
import com.hcc.repository.core.jdbc.BasicRowMapper;
import com.hcc.repository.core.metadata.TableColumnInfo;
import com.hcc.repository.core.metadata.TableInfo;
import com.hcc.repository.core.metadata.TableInfoHelper;
import com.hcc.repository.core.utils.ReflectUtils;
import com.hcc.repository.core.utils.StrUtils;
import org.springframework.jdbc.support.JdbcUtils;

import java.lang.reflect.Field;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 * 普通对象RowMapper，使用字段名称作为映射
 *
 * @author hushengjun
 * @date 2023/4/26
 */
public class PlainEntityRowMapper<T> implements BasicRowMapper<T> {

    private final Class<T> entityClass;
    private final Map<String, Field> fieldNameMap;
    private final Map<String, Field> fieldUnderlineNameMap;

    public PlainEntityRowMapper(Class<T> entityClass) {
        this.entityClass = entityClass;
        List<Field> fields = ReflectUtils.getAllDeclaredFields(entityClass);
        fieldNameMap = fields.stream().collect(Collectors.toMap(Field::getName, Function.identity()));
        fieldUnderlineNameMap = fields.stream().collect(Collectors.toMap(f -> StrUtils.humpToUnderline(f.getName()), Function.identity()));
    }

    @Override
    public T resultMap(ResultSet rs, int rowNum) throws SQLException {
        T instance = ReflectUtils.newInstance(entityClass);

        ResultSetMetaData rsMetaData = rs.getMetaData();
        int columnCount = rsMetaData.getColumnCount();
        for (int index = 1; index <= columnCount; index++) {
            String columnName = this.getColumnName(rsMetaData, index);

            Object columnValue = JdbcUtils.getResultSetValue(rs, index);
            if (columnValue == null) {
                continue;
            }

            Field field = fieldNameMap.get(columnName);
            if (field == null && !strictMode()) {
                field = fieldUnderlineNameMap.get(columnName);
            }
            if (field == null) {
                continue;
            }

            // 反射赋值
            ReflectUtils.setValue(instance, field, this.getColumnValue(rs, columnName, index, field.getType()));
        }

        return instance;
    }

    /**
     * 获取列值
     * @param rs
     * @param columnName
     * @param index
     * @param fieldClass
     * @return
     * @throws SQLException
     */
    protected Object getColumnValue(ResultSet rs, String columnName, int index, Class<?> fieldClass) throws SQLException {
        Object columnValue = JdbcUtils.getResultSetValue(rs, index);
        if (columnValue == null) {
            return null;
        }
        Object targetValue;
        ValueConverter<?> converter = ConverterFactory.getConverter(fieldClass);
        if (converter != null) {
            // 可以获取到转换器，则使用转换器转换
            targetValue = converter.convert(columnValue, fieldClass);
        } else {
            // 否则使用的是ResultSet提供的方法获取值
            targetValue = JdbcUtils.getResultSetValue(rs, index, fieldClass);
        }

        return targetValue;
    }

    /**
     * 严格模式，字段与列表必须一致，否则可以转下划线一致即可映射
     * @return
     */
    protected boolean strictMode() {
        return true;
    }

}
