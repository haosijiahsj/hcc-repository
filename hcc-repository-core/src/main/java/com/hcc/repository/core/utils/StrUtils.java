package com.hcc.repository.core.utils;

import com.hcc.repository.core.constants.StrPool;

import java.text.MessageFormat;

/**
 * 字符串工具类
 *
 * @author shengjun.hu
 * @date 2022/11/16
 */
public class StrUtils {

    private static final String UNDERLINE = "_";

    private StrUtils() {}

    public static boolean isEmpty(String str) {
        return str == null || str.length() == 0;
    }

    public static boolean isNotEmpty(String str) {
        return !isEmpty(str);
    }

    /**
     * 从驼峰转为下划线分割
     * @param str
     * @return
     */
    public static String humpToUnderline(String str) {
        if (isEmpty(str)) {
            return str;
        }

        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < str.length(); i++) {
            char c = str.charAt(i);
            // 第一位是大写，不加下划线
            if (Character.isUpperCase(c) && i == 0) {
                sb.append(Character.toLowerCase(c));
                continue;
            }
            if (Character.isUpperCase(c)) {
                sb.append(UNDERLINE).append(Character.toLowerCase(c));
            } else {
                sb.append(c);
            }
        }

        return sb.toString();
    }

    public static String joinSpace(String...strs) {
        return String.join(StrPool.SPACE, strs);
    }

    public static String join(CharSequence delimiter, CharSequence... elements) {
        return String.join(delimiter, elements);
    }

    public static String join(CharSequence delimiter, Iterable<? extends CharSequence> elements) {
        return String.join(delimiter, elements);
    }

    public static String format(String pattern, Object...args) {
        return MessageFormat.format(pattern, args);
    }

}
