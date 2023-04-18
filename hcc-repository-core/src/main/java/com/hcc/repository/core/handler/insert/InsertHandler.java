package com.hcc.repository.core.handler.insert;

import com.hcc.repository.annotation.IEnum;
import com.hcc.repository.annotation.IdGenerator;
import com.hcc.repository.annotation.IdType;
import com.hcc.repository.core.conditions.ICondition;
import com.hcc.repository.core.conditions.insert.DefaultInsertCondition;
import com.hcc.repository.core.handler.AbstractMethodHandler;
import com.hcc.repository.core.metadata.TableColumnInfo;
import com.hcc.repository.core.metadata.TableInfoHelper;
import com.hcc.repository.core.utils.Assert;
import com.hcc.repository.core.utils.Pair;
import com.hcc.repository.core.utils.ReflectUtils;
import org.springframework.util.NumberUtils;

import java.lang.reflect.Constructor;
import java.util.List;
import java.util.Map;
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
        return assembleCondition(getFirstArg());
    }

    /**
     * 组装Condition
     * @param entity
     * @return
     */
    protected ICondition<?> assembleCondition(Object entity) {
        DefaultInsertCondition<?> condition = new DefaultInsertCondition<>(entity);
        List<TableColumnInfo> columnInfos = TableInfoHelper.getColumnInfosWithOutIdColumn(entityClass);
        columnInfos.forEach(c -> {
            Object value = ReflectUtils.getValue(entity, c.getField());
            // 转换
            Object targetValue = value;
            if (c.needConvert()) {
                targetValue = ReflectUtils.newInstance(c.getConverter()).convertToColumn(value);
            } else if (c.isAssignableFromIEnum()) {
                targetValue = ((IEnum<?>) value).getValue();
            }
            condition.value(c.getColumnName(), targetValue);
        });

        TableColumnInfo idColumnInfo = TableInfoHelper.getIdColumnInfo(entityClass);
        if (idColumnInfo != null && IdType.GENERATED.equals(idColumnInfo.getIdType())) {
            condition.value(idColumnInfo.getColumnName(), getIdValue(idColumnInfo, entity));
        }

        return condition;
    }

    @Override
    protected Object executeSql(String sql, Object[] args) {
        Object firstArg = getFirstArg();
        TableColumnInfo idColumnInfo = TableInfoHelper.getIdColumnInfo(entityClass);
        if (idColumnInfo != null && IdType.IDENTITY.equals(idColumnInfo.getIdType())) {
            Pair<Number, Integer> pair = jdbcTemplateProxy.updateForKey(sql, args);
            Object value = NumberUtils.convertNumberToTargetClass(pair.getLeft(), (Class<? extends Number>) idClass);
            // 回填id到实体中
            ReflectUtils.setValue(firstArg, idColumnInfo.getField(), value);

            return pair.getRight();
        }

        return jdbcTemplateProxy.update(sql, args);
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
        if (IdType.ASSIGNED.equals(idType)) {
            // 用户指定值
            idValue = ReflectUtils.getValue(entity, idColumnInfo.getField());
        } else if (IdType.GENERATED.equals(idType)) {
            idValue = newInstance(idColumnInfo.getGenerator(), idColumnInfo.isUseSingletonIdGenerator()).nextId();
        }
        if (idValue != null) {
            // 回填id到实体中
            ReflectUtils.setValue(entity, idColumnInfo.getField(), idValue);
        }

        return idValue;
    }

    /**
     * 实例化IdGenerator
     * @param generatorClass
     * @param useSingletonIdGenerator
     * @return
     */
    private IdGenerator<?> newInstance(Class<? extends IdGenerator> generatorClass, boolean useSingletonIdGenerator) {
        if (useSingletonIdGenerator) {
            IdGenerator<?> idGeneratorCache = ID_GENERATOR_CACHE.get(generatorClass);
            if (idGeneratorCache != null) {
                return idGeneratorCache;
            }
        }
        Constructor<?>[] constructors = generatorClass.getDeclaredConstructors();
        Assert.isTrue(constructors.length >= 1, String.format("%s 无构造方法", generatorClass.getName()));

        IdGenerator<?> idGenerator = null;
        for (Constructor<?> constructor : constructors) {
            int parameterCount = constructor.getParameterCount();
            if (parameterCount == 1) {
                try {
                    idGenerator = (IdGenerator<?>) constructor.newInstance(configuration);
                    break;
                } catch (Exception e) {
                    throw new RuntimeException(e);
                }
            }
        }

        // 默认返回无参的
        if (idGenerator == null) {
            idGenerator = ReflectUtils.newInstance(generatorClass);
        }
        if (useSingletonIdGenerator) {
            ID_GENERATOR_CACHE.put(generatorClass, idGenerator);
        }

        return idGenerator;
    }

}
