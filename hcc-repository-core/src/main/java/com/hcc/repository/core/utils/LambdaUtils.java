package com.hcc.repository.core.utils;

/**
 * LambdaUtils
 *
 * @author hushengjun
 * @date 2023/3/25
 */
public class LambdaUtils {

    public static String getFieldNameFromSetterGetterMethodName(String methodName) {
        if (methodName == null) {
            return null;
        }

        String columnName;
        if (methodName.startsWith("is")) {
            columnName = methodToField(methodName, "is");
        } else if (methodName.startsWith("get")) {
            columnName = methodToField(methodName, "get");
        } else if (methodName.startsWith("set")) {
            columnName = methodToField(methodName, "set");
        } else {
            columnName = methodName;
        }

        return columnName;
    }

    private static String methodToField(String methodName, String prefix) {
        int index = methodName.indexOf(prefix) + prefix.length();
        char c = methodName.charAt(index);
        return String.valueOf(c).toLowerCase() + methodName.substring(index + 1);
    }

}
