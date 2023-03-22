package com.hcc.repository.core.utils;

/**
 * ArrayUtils
 *
 * @author hushengjun
 * @date 2023/3/21
 */
public class ArrayUtils {

    private ArrayUtils() {
    }

    public static boolean isEmpty(Object[] array) {
        return array == null || array.length == 0;
    }

    public static boolean isNotEmpty(Object[] array) {
        return !isEmpty(array);
    }

}

