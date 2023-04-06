package com.hcc.repository.extension.converter;

import com.hcc.repository.annotation.IConverter;

/**
 * AbstractEnumConverter
 *
 * @author hushengjun
 * @date 2023/4/4
 */
public abstract class AbstractEnumConverter<E extends Enum<E>, COL> implements IConverter<E, COL> {
    
    @Override
    public COL convertToColumn(E attribute) {
        if (attribute == null) {
            return null;
        }

        return toColumn(attribute);
    }

    @Override
    public E convertToAttribute(COL column) {
        if (column == null) {
            return null;
        }

        return toEnum(column);
    }

    public abstract COL toColumn(E attribute);

    public abstract E toEnum(COL column);

}
