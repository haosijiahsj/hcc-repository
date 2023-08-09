package com.hcc.repository.annotation;

import java.lang.reflect.Field;

/**
 * 字典回写
 *
 * @author hushengjun
 * @date 2023/8/9
 */
@SuppressWarnings("unchecked")
public interface DictPropSetListener<T, V> extends PropSetListener {

    @Override
    default Object onPropSet(Object entity, Object value, String propName, String columnName) {
        if (value == null) {
            return null;
        }

        Object dictVal = getDictValue((T) entity, columnName, (V) value);
        if (dictVal != null) {
            try {
                Field field = entity.getClass().getDeclaredField(getDictFieldName());
                field.setAccessible(true);
                field.set(entity, dictVal);
                if (!field.isAccessible()) {
                    field.setAccessible(false);
                }
            } catch (Exception e) {
                throw new RuntimeException(e);
            }
        }

        return value;
    }

    /**
     * 回写的字段名称
     * @return
     */
    String getDictFieldName();

    /**
     * 获取字典值
     * @param columName
     * @param value
     * @return
     */
    Object getDictValue(T entity, String columName, V value);

}
