package com.hcc.repository.core.constants;

/**
 * StrPool
 *
 * @author hushengjun
 * @date 2023/3/25
 */
public class StrPool {

    public static final String EMPTY = "";
    public static final String SPACE = " ";
    public static final String POINT = ".";
    public static final String UNDERLINE = "_";
    public static final String SHORT_LINE = "-";
    public static final String COMMA = ",";
    public static final String COMMA_SPACE = ", ";
    public static final String COLON = ":";
    public static final String L_BRACKET = "(";
    public static final String R_BRACKET = ")";
    public static final String PLACEHOLDER_PREFIX = "#{";
    public static final String PLACEHOLDER_SUFFIX = "}";
    public static final String HASH = "#";
    public static final String DOLLAR = "$";
    public static final String QU_MASK = "?";
    public static final String SEMICOLON = ";";
    public static final String DOT = ".";

    public static String getPlaceholder(String name) {
        return String.format("%s%s%s", PLACEHOLDER_PREFIX, name, PLACEHOLDER_SUFFIX);
    }

}
