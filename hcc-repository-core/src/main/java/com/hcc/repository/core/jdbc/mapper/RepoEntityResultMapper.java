package com.hcc.repository.core.jdbc.mapper;

import com.hcc.repository.annotation.IConverter;
import com.hcc.repository.annotation.PropSetListener;
import com.hcc.repository.core.convert.EnumNameConverter;
import com.hcc.repository.core.convert.EnumOrdinalConverter;
import com.hcc.repository.core.convert.IEnumConverter;
import com.hcc.repository.core.jdbc.ResultMapper;
import com.hcc.repository.core.metadata.TableColumnInfo;
import com.hcc.repository.core.metadata.TableInfo;
import com.hcc.repository.core.metadata.TableInfoHelper;
import com.hcc.repository.core.utils.ConstructorUtils;
import com.hcc.repository.core.utils.JdbcUtils;
import com.hcc.repository.core.utils.ReflectUtils;
import com.hcc.repository.core.utils.StrUtils;

import java.lang.reflect.Constructor;
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
            TableColumnInfo columnInfo = columnNameColumnInfoMap.getOrDefault(columnName, fieldNameColumnInfoMap.get(columnName));
            if (columnInfo == null) {
                continue;
            }
            Object columnValue = JdbcUtils.getResultSetValue(rs, index);
            if (columnValue == null) {
                continue;
            }

            Field field = columnInfo.getField();
            Class<?> fieldTypeClass = field.getType();
            Object targetValue;

            Class<? extends IConverter> converter = null;
            if (columnInfo.needConvert()) {
                // 用户自定义转换器
                converter = columnInfo.getConverter();
            } else if (columnInfo.isIEnum()) {
                // IEnum枚举处理
                converter = IEnumConverter.class;
            } else if (columnInfo.isEnum()) {
                // 枚举处理
                if (String.class.equals(columnInfo.getFieldType())) {
                    converter = EnumNameConverter.class;
                } else if (Integer.class.equals(columnInfo.getFieldType()) || int.class.equals(columnInfo.getFieldType())) {
                    converter = EnumOrdinalConverter.class;
                }
            }

            if (converter != null) {
                targetValue = this.newInstanceConverter(converter, columnInfo).convertToAttribute(columnValue);
            } else {
                targetValue = JdbcUtils.getResultSetValue(rs, index, fieldTypeClass);
            }

            // 执行监听器
            if (tableInfo.needPropSet()) {
                PropSetListener propSetListener = ReflectUtils.newInstanceForCache(tableInfo.getPropSet());
                if (propSetListener.test(instance, targetValue, field.getName(), columnName)) {
                    targetValue = propSetListener.onPropSet(instance, targetValue, field.getName(), columnName);
                }
            }

            // 反射赋值
            ReflectUtils.setValue(instance, field, targetValue);
        }

        return instance;
    }

    /**
     * 实例化converter
     * @param converterClass
     * @param columnInfo
     * @return
     */
    private IConverter newInstanceConverter(Class<? extends IConverter> converterClass, TableColumnInfo columnInfo) {
        Constructor<?>[] constructors = converterClass.getDeclaredConstructors();
        for (Constructor<?> constructor : constructors) {
            if (constructor.getParameterCount() > 1) {
                continue;
            }

            if (constructor.getParameterCount() == 1) {
                Class<?> parameterType = constructor.getParameterTypes()[0];
                if (Class.class.equals(parameterType)) {
                    return (IConverter) ConstructorUtils.newInstance(constructor, columnInfo.getFieldType());
                } else if (Field.class.equals(parameterType)) {
                    return (IConverter) ConstructorUtils.newInstance(constructor, columnInfo.getField());
                }
            } else if (constructor.getParameterCount() == 0) {
                return ReflectUtils.newInstance(converterClass);
            }
        }

        throw new IllegalArgumentException(StrUtils.format("converter: {0} 没有合适的构造器，如：public Converter() {}, public Converter(Class<?> clazz) {}, public Converter(Field field) {}", converterClass.getName()));

//        return Optional.ofNullable(ReflectUtils.matchConstruct(converterClass, Class.class))
//                .map(c -> (IConverter) ConstructorUtils.newInstance(c, columnInfo.getFieldType()))
//                .orElseGet(() -> ReflectUtils.newInstance(converterClass));
    }

}
