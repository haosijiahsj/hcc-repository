package com.hcc.repository.extension.converter.enums;

/**
 * EnumNameConverter
 *
 * @author hushengjun
 * @date 2023/7/26
 */
public class EnumNameConverter<E extends Enum<E>> implements EnumConverter<E, String> {

    private final Class<E> enumClass;

    public EnumNameConverter(Class<E> enumClass) {
        this.enumClass = enumClass;
    }

    @Override
    public String toColumn(E attribute) {
        return attribute.name();
    }

    @Override
    public E toEnum(String column) {
        E[] enumConstants = enumClass.getEnumConstants();
        for (E enumConstant : enumConstants) {
            if (enumConstant.name().equals(column)) {
                return enumConstant;
            }
        }

        throw new IllegalArgumentException(String.format("name value: [%s]未被定义在Enum: %s中", column, enumClass.getName()));
    }

}
