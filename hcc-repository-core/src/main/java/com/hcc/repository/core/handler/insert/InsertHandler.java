package com.hcc.repository.core.handler.insert;

import com.hcc.repository.annotation.AutoFillContext;
import com.hcc.repository.annotation.AutoFillStrategy;
import com.hcc.repository.annotation.IConverter;
import com.hcc.repository.annotation.IdGenerator;
import com.hcc.repository.annotation.IdType;
import com.hcc.repository.core.conditions.ICondition;
import com.hcc.repository.core.conditions.insert.DefaultInsertCondition;
import com.hcc.repository.core.convert.IEnumConverter;
import com.hcc.repository.core.handler.AbstractMethodHandler;
import com.hcc.repository.core.metadata.TableColumnInfo;
import com.hcc.repository.core.metadata.TableInfo;
import com.hcc.repository.core.metadata.TableInfoHelper;
import com.hcc.repository.core.spring.config.RepositoryConfiguration;
import com.hcc.repository.core.utils.ConstructorUtils;
import com.hcc.repository.core.utils.Pair;
import com.hcc.repository.core.utils.ReflectUtils;
import org.springframework.util.NumberUtils;

import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.ConcurrentHashMap;

/**
 * InsertHandler
 *
 * @author hushengjun
 * @date 2023/3/21
 */
@SuppressWarnings("unchecked")
public class InsertHandler extends AbstractMethodHandler {

    private static final Map<Class<?>, IdGenerator<?>> ID_GENERATOR_CACHE;

    static {
        ID_GENERATOR_CACHE = new ConcurrentHashMap<>(32);
    }

    @Override
    protected ICondition<?> prepareCondition() {
        return buildCondition(getFirstArg());
    }

    /**
     * 组装Condition
     * @param entity
     * @return
     */
    protected ICondition<?> buildCondition(Object entity) {
        DefaultInsertCondition<?> condition = new DefaultInsertCondition<>(entity);
        List<TableColumnInfo> columnInfos = TableInfoHelper.getColumnInfosWithOutIdColumn(entityClass);
        columnInfos.forEach(c -> condition.value(c.getColumnName(), this.getColumnValue(entity, c)));

        TableColumnInfo idColumnInfo = TableInfoHelper.getIdColumnInfo(entityClass);
        if (idColumnInfo != null && (IdType.GENERATE.equals(idColumnInfo.getIdType()) || IdType.SPECIFY.equals(idColumnInfo.getIdType()))) {
            condition.value(idColumnInfo.getColumnName(), getIdValue(idColumnInfo, entity));
        }

        return condition;
    }

    @Override
    protected Object executeSql(String sql, Object[] args) {
        Object firstArg = getFirstArg();
        TableColumnInfo idColumnInfo = TableInfoHelper.getIdColumnInfo(entityClass);
        if (idColumnInfo != null && IdType.AUTO.equals(idColumnInfo.getIdType())) {
            Pair<Number, Integer> pair = jdbcOperations.updateForKey(sql, args);
            Object value = NumberUtils.convertNumberToTargetClass(pair.getLeft(), (Class<? extends Number>) idClass);
            // 回填id到实体中
            ReflectUtils.setValue(firstArg, idColumnInfo.getField(), value);

            return pair.getRight();
        }

        return jdbcOperations.update(sql, args);
    }

    /**
     * 获取id值
     * @param idColumnInfo
     * @param entity
     * @return
     */
    protected Object getIdValue(TableColumnInfo idColumnInfo, Object entity) {
        Object idValue = null;
        IdType idType = idColumnInfo.getIdType();
        if (IdType.SPECIFY.equals(idType)) {
            // 用户指定值
            idValue = ReflectUtils.getValue(entity, idColumnInfo.getField());
        } else if (IdType.GENERATE.equals(idType)) {
            idValue = newInstanceGenerator(idColumnInfo.getGenerator(), idColumnInfo.isUseSingletonIdGenerator()).nextId();
        }
        if (idValue != null) {
            // 回填id到实体中
            ReflectUtils.setValue(entity, idColumnInfo.getField(), idValue);
        }

        return idValue;
    }

    /**
     * 获取列值
     * @param entity
     * @param columnInfo
     * @return
     */
    protected Object getColumnValue(Object entity, TableColumnInfo columnInfo) {
        Object value = ReflectUtils.getValue(entity, columnInfo.getField());
        // 转换
        Object targetValue = value;
        if (value != null) {
            Class<? extends IConverter> converter = null;
            if (columnInfo.needConvert()) {
                converter = columnInfo.getConverter();
            } else if (columnInfo.isAssignableFromIEnum()) {
                converter = IEnumConverter.class;
            }
            if (converter != null) {
                targetValue = this.newInstanceConverter(converter, columnInfo.getField().getType()).convertToColumn(value);
            }
        }
        // 自动填充处理
        if (targetValue == null && columnInfo.needAutoFillInsert()) {
            targetValue = this.getInsertAutoFillValue(TableInfoHelper.getTableInfo(entityClass), columnInfo);
        }

        return targetValue;
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

    /**
     * 获取填充值
     * @param tableInfo
     * @param columnInfo
     * @return
     */
    protected Object getInsertAutoFillValue(TableInfo tableInfo, TableColumnInfo columnInfo) {
        AutoFillContext context = new AutoFillContext();
        context.setFieldName(columnInfo.getFieldName());
        context.setColumnName(columnInfo.getColumnName());
        context.setFieldType(columnInfo.getField().getType());
        context.setTableName(tableInfo.getTableName());
        context.setEntityClass(tableInfo.getClazz());

        AutoFillStrategy autoFillStrategy = ReflectUtils.newInstanceForCache(columnInfo.getInsertStrategy());
        if (!autoFillStrategy.autoFill(context)) {
            return null;
        }

        return autoFillStrategy.fill(context);
    }

    /**
     * 实例化IdGenerator
     * @param generatorClass
     * @param useSingletonIdGenerator
     * @return
     */
    private IdGenerator<?> newInstanceGenerator(Class<? extends IdGenerator> generatorClass, boolean useSingletonIdGenerator) {
        if (useSingletonIdGenerator) {
            IdGenerator<?> idGeneratorCache = ID_GENERATOR_CACHE.get(generatorClass);
            if (idGeneratorCache != null) {
                return idGeneratorCache;
            }
        }

        // 实例化
        IdGenerator<?> idGenerator = Optional.ofNullable(ReflectUtils.matchConstruct(generatorClass, RepositoryConfiguration.class))
                .map(c -> (IdGenerator<?>) ConstructorUtils.newInstance(c, configuration))
                .orElseGet(() -> ReflectUtils.newInstance(generatorClass));

        if (useSingletonIdGenerator) {
            ID_GENERATOR_CACHE.put(generatorClass, idGenerator);
        }

        return idGenerator;
    }

}
