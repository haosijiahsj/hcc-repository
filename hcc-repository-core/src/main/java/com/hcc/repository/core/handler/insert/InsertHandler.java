package com.hcc.repository.core.handler.insert;

import com.hcc.repository.annotation.IdGenerator;
import com.hcc.repository.annotation.IdType;
import com.hcc.repository.core.conditions.insert.DefaultInsertCondition;
import com.hcc.repository.core.handler.AbstractMethodHandler;
import com.hcc.repository.core.metadata.TableColumnInfo;
import com.hcc.repository.core.metadata.TableInfoHelper;
import com.hcc.repository.core.utils.Pair;
import com.hcc.repository.core.utils.ReflectUtils;
import org.springframework.util.NumberUtils;

import java.util.List;
import java.util.UUID;

/**
 * InsertHandler
 *
 * @author hushengjun
 * @date 2023/3/21
 */
public class InsertHandler extends AbstractMethodHandler {

    @Override
    protected Object handleMethod() throws Exception {
        Object firstArg = getFirstArg();
        if (firstArg == null) {
            throw new IllegalArgumentException("插入参数不能为空！");
        }

        DefaultInsertCondition<?> condition = this.buildInsertCondition(firstArg);
        TableColumnInfo idColumnInfo = TableInfoHelper.getIdColumnInfo(entityClass);
        if (idColumnInfo != null && !IdType.IDENTITY.equals(idColumnInfo.getIdType())) {
            Pair<Number, Integer> pair = jdbcTemplateWrapper.namedUpdateForKey(condition.getSqlInsert(), condition.getColumnValuePairs());
            Object value = NumberUtils.convertNumberToTargetClass(pair.getLeft(), (Class<? extends Number>) idClass);
            // 回填id到实体中
            ReflectUtils.setValue(firstArg, idColumnInfo.getField(), value);

            return pair.getRight();
        }

        return jdbcTemplateWrapper.namedUpdate(condition.getSqlInsert(), condition.getColumnValuePairs());
    }

    protected DefaultInsertCondition<?> buildInsertCondition(Object firstArg) {
        DefaultInsertCondition<?> condition = new DefaultInsertCondition<>(firstArg);
        List<TableColumnInfo> columnInfos = TableInfoHelper.getColumnInfosWithOutIdColumn(entityClass);
        columnInfos.forEach(c -> {
            Object value = ReflectUtils.getValue(firstArg, c.getField());
            // 转换
            Object targetValue = value;
            if (c.needConvert()) {
                targetValue = ReflectUtils.newInstance(c.getConverter()).convertToColumn(value);
            }
            condition.value(c.getColumnName(), targetValue);
        });

        TableColumnInfo idColumnInfo = TableInfoHelper.getIdColumnInfo(entityClass);
        if (idColumnInfo != null) {
            if (!IdType.IDENTITY.equals(idColumnInfo.getIdType())) {
                condition.value(idColumnInfo.getColumnName(), getIdValue(idColumnInfo, firstArg));
            }
        }

        return condition;
    }

    protected Object getIdValue(TableColumnInfo idColumnInfo, Object entity) {
        Object idValue = null;
        IdType idType = idColumnInfo.getIdType();
        if (IdType.ASSIGNED.equals(idType)) {
            // 用户指定值
            idValue = ReflectUtils.getValue(entity, idColumnInfo.getField());
        } else if (IdType.UUID.equals(idType)) {
            idValue = UUID.randomUUID().toString().replaceAll("-", "");
        } else if (IdType.GENERATED.equals(idType)) {
            Class<? extends IdGenerator> generator = idColumnInfo.getGenerator();
            try {
                idValue = generator.newInstance().nextId();
            } catch (InstantiationException | IllegalAccessException e) {
                throw new RuntimeException(e);
            }
        }
        if (idValue != null) {
            // 回填id到实体中
            ReflectUtils.setValue(entity, idColumnInfo.getField(), idValue);
        }

        return idValue;
    }

}
