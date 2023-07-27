package com.hcc.repository.core.convert;

import com.hcc.repository.annotation.IConverter;

/**
 * 枚举转换器
 *
 * @author hushengjun
 * @date 2023/4/4
 */
public interface EnumConverter<E extends Enum<E>, COL> extends IConverter<E, COL> {
    
    @Override
    default COL convertToColumn(E attribute) {
        if (attribute == null) {
            return null;
        }

        return toColumn(attribute);
    }

    @Override
    default E convertToAttribute(COL column) {
        if (column == null) {
            return null;
        }

        return toEnum(column);
    }

    /**
     * 到列的值
     * @param attribute
     * @return
     */
    COL toColumn(E attribute);

    /**
     * 到枚举的值
     * @param column
     * @return
     */
    E toEnum(COL column);

}
