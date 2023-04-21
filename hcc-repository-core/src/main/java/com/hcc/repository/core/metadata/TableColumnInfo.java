package com.hcc.repository.core.metadata;

import com.hcc.repository.annotation.AutoFillStrategy;
import com.hcc.repository.annotation.UnknownConverter;
import com.hcc.repository.annotation.IConverter;
import com.hcc.repository.annotation.IEnum;
import com.hcc.repository.annotation.IdGenerator;
import com.hcc.repository.annotation.IdType;
import com.hcc.repository.annotation.LogicDelValueType;
import com.hcc.repository.annotation.UnknownFillStrategy;
import lombok.Data;

import java.lang.reflect.Field;

/**
 * TableColumnInfo
 *
 * @author hushengjun
 * @date 2023/3/6
 */
@Data
public class TableColumnInfo {

    private String columnName;
    private String fieldName;
    private Field field;

    // id相关信息
    private boolean primaryKey = false;
    private IdType idType;
    private Class<? extends IdGenerator> generator;
    private Class<? extends IConverter> converter = UnknownConverter.class;
    private boolean useSingletonIdGenerator = true;

    // 逻辑删除
    private boolean logicDelete = false;
    private String logicDelVal;
    private String logicNotDelVal;
    private LogicDelValueType logicDelValueType;

    // 填充策略
    private Class<? extends AutoFillStrategy> insertStrategy = UnknownFillStrategy.class;
    private Class<? extends AutoFillStrategy> updateStrategy = UnknownFillStrategy.class;

    public boolean needConvert() {
        return !UnknownConverter.class.equals(converter) && !IConverter.class.equals(converter);
    }

    public boolean needAutoFillInsert() {
        return !UnknownFillStrategy.class.equals(insertStrategy) && !AutoFillStrategy.class.equals(insertStrategy);
    }

    public boolean needAutoFillUpdate() {
        return !UnknownFillStrategy.class.equals(updateStrategy) && !AutoFillStrategy.class.equals(updateStrategy);
    }

    public boolean isEnum() {
        return field.getType().isEnum();
    }

    public boolean isAssignableFromIEnum() {
        return isEnum() && IEnum.class.isAssignableFrom(field.getType());
    }

}
