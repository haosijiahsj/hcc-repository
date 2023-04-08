package com.hcc.repository.core.metadata;

import com.hcc.repository.annotation.DefaultConverter;
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
    private boolean primaryKey = false;
    private IdType idType;
    private Class<? extends IdGenerator> generator;
    private Class<? extends IConverter> converter = DefaultConverter.class;

    // 逻辑删除
    private boolean logicDelete = false;
    private String logicDelVal;
    private String logicNotDelVal;
    private LogicDelValueType logicDelValueType;

    public boolean needConvert() {
        return !DefaultConverter.class.equals(converter) && !IConverter.class.equals(converter);
    }

    public boolean isEnum() {
        return field.getType().isEnum();
    }

    public boolean isAssignableFromIEnum() {
        return isEnum() && IEnum.class.isAssignableFrom(field.getType());
    }

}
