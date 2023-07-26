package com.hcc.repository.core.convert;

import com.hcc.repository.annotation.IConverter;
import com.hcc.repository.annotation.IEnum;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.Objects;

/**
 * IEnumConverter
 *
 * @author hushengjun
 * @date 2023/7/26
 */
public class IEnumConverter<T extends Serializable> implements IConverter<IEnum<T>, T> {

    private final Class<?> enumClass;

    public IEnumConverter(Class<?> enumClass) {
        this.enumClass = enumClass;
    }

    @Override
    @SuppressWarnings("unchecked")
    public IEnum<T> convertToAttribute(T column) {
        if (column == null) {
            return null;
        }

        Object[] enumConstants = enumClass.getEnumConstants();
        for (Object e : enumConstants) {
            IEnum<T> iEnum = (IEnum<T>) e;
            T value = iEnum.getValue();
            if (value == null) {
                continue;
            }

            String columnStr = column.toString();
            if (value instanceof BigDecimal && new BigDecimal(columnStr).compareTo((BigDecimal) value) == 0) {
                return iEnum;
            }
            if (Objects.equals(column.toString(), value.toString())) {
                return iEnum;
            }
        }

        throw new IllegalArgumentException(String.format("value: [%s]未被定义在IEnum: %s中", column, enumClass.getName()));
    }

    @Override
    public T convertToColumn(IEnum<T> attribute) {
        return attribute != null ? attribute.getValue() : null;
    }

}
