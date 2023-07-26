package com.hcc.repository.extension.converter.enums;

/**
 * EnumOrdinalConverter
 *
 * @author hushengjun
 * @date 2023/7/26
 */
public class EnumOrdinalConverter<E extends Enum<E>> implements EnumConverter<E, Integer> {

    private final Class<E> enumClass;

    public EnumOrdinalConverter(Class<E> enumClass) {
        this.enumClass = enumClass;
    }

    @Override
    public Integer toColumn(E attribute) {
        return attribute.ordinal();
    }

    @Override
    public E toEnum(Integer column) {
        E[] enumConstants = enumClass.getEnumConstants();
        for (E enumConstant : enumConstants) {
            if (enumConstant.ordinal() == column) {
                return enumConstant;
            }
        }

        throw new IllegalArgumentException(String.format("ordinal value: [%s]未被定义在Enum: %s中", column, enumClass.getName()));
    }

}
