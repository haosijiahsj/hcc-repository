package com.hcc.repository.core.metadata;

import com.hcc.repository.annotation.AutoFillStrategy;
import com.hcc.repository.annotation.Constants;
import com.hcc.repository.annotation.IConverter;
import com.hcc.repository.annotation.IEnum;
import com.hcc.repository.annotation.IdGenerator;
import com.hcc.repository.annotation.IdType;
import com.hcc.repository.annotation.LogicDelValueType;
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
    private Class<? extends IConverter> converter = Constants.UnknownConverter.class;
    private boolean useSingletonIdGenerator = true;

    // 逻辑删除
    private boolean logicDelete = false;
    private String logicDelVal;
    private String logicNotDelVal;
    private LogicDelValueType logicDelValueType;

    // 乐观锁
    private boolean version = false;

    // 填充策略
    private Class<? extends AutoFillStrategy> insertStrategy = Constants.UnknownFillStrategy.class;
    private Class<? extends AutoFillStrategy> updateStrategy = Constants.UnknownFillStrategy.class;

    public boolean needConvert() {
        return !Constants.UnknownConverter.class.equals(converter) && !IConverter.class.equals(converter);
    }

    public boolean needAutoFillInsert() {
        return !Constants.UnknownFillStrategy.class.equals(insertStrategy) && !AutoFillStrategy.class.equals(insertStrategy);
    }

    public boolean needAutoFillUpdate() {
        return !Constants.UnknownFillStrategy.class.equals(updateStrategy) && !AutoFillStrategy.class.equals(updateStrategy);
    }

    public Class<?> getFieldType() {
        return field.getType();
    }

    public boolean isEnum() {
        return field.getType().isEnum();
    }

    public boolean isAssignableFromIEnum() {
        return isEnum() && IEnum.class.isAssignableFrom(getFieldType());
    }

}
