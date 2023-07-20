package com.hcc.repository.core.utils;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;

/**
 * ConstructorUtils
 *
 * @author hushengjun
 * @date 2023/7/20
 */
public class ConstructorUtils {

    private ConstructorUtils() {}

    /**
     * 通过构造方法实例化
     * @param constructor
     * @param initArgs
     * @return
     * @param <T>
     */
    public static <T> T newInstance(Constructor<T> constructor, Object...initArgs) {
        try {
            return constructor.newInstance(initArgs);
        } catch (InstantiationException | IllegalAccessException | InvocationTargetException e) {
            throw new RuntimeException(e);
        }
    }

}
