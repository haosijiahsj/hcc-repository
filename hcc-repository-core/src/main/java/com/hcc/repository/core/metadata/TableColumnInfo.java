package com.hcc.repository.core.metadata;

import com.hcc.repository.annotation.DefaultConverter;
import com.hcc.repository.annotation.IConverter;
import com.hcc.repository.annotation.IdGenerator;
import com.hcc.repository.annotation.IdType;
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

    public boolean needConvert() {
        return !DefaultConverter.class.equals(converter) && !IConverter.class.equals(converter);
    }

}
