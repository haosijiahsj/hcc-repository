package com.hcc.repository.core.utils;

import java.text.MessageFormat;

/**
 * Assert
 *
 * @author hushengjun
 * @date 2023/3/22
 */
public class Assert {

    public static void isNotNull(Object object, String message) {
        if (object == null) {
            throw new IllegalArgumentException(message);
        }
    }

    public static void isNotNull(Object object, String message, Object...args) {
        if (object == null) {
            throw new IllegalArgumentException(MessageFormat.format(message, args));
        }
    }

    public static void isNull(Object object, String message) {
        if (object != null) {
            throw new IllegalArgumentException(message);
        }
    }

    public static void isNull(Object object, String message, Object...args) {
        if (object != null) {
            throw new IllegalArgumentException(MessageFormat.format(message, args));
        }
    }

    public static void isTrue(boolean expression, String message) {
        if (!expression) {
            throw new IllegalArgumentException(message);
        }
    }

    public static void isTrue(boolean expression, String message, Object...args) {
        if (!expression) {
            throw new IllegalArgumentException(MessageFormat.format(message, args));
        }
    }

    public static void isFalse(boolean expression, String message) {
        if (expression) {
            throw new IllegalArgumentException(message);
        }
    }

    public static void isFalse(boolean expression, String message, Object...args) {
        if (expression) {
            throw new IllegalArgumentException(MessageFormat.format(message, args));
        }
    }

}
