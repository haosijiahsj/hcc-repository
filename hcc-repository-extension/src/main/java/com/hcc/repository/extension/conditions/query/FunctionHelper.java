package com.hcc.repository.extension.conditions.query;

import org.springframework.util.NumberUtils;

import java.math.BigDecimal;
import java.util.function.Function;

/**
 * FunctionHelper
 *
 * @author hushengjun
 * @date 2023/4/8
 */
public class FunctionHelper {

    public static Function<Object, Long> toLong() {
        return o -> {
            if (o == null) {
                return null;
            }
            return Long.valueOf(o.toString());
        };
    }

    public static Function<Object, Integer> toInteger() {
        return o -> {
            if (o == null) {
                return null;
            }
            return Integer.valueOf(o.toString());
        };
    }

    public static Function<Object, BigDecimal> toBigDecimal() {
        return o -> {
            if (o == null) {
                return null;
            }
            return new BigDecimal(o.toString());
        };
    }

    public static <T extends Number> Function<Object, T> toNumber(Class<T> clazz) {
        return o -> {
            if (o == null) {
                return null;
            }
            return NumberUtils.parseNumber(o.toString(), clazz);
        };
    }

    public static Function<Object, String> toStr() {
        return o -> {
            if (o == null) {
                return null;
            }
            return o.toString();
        };
    }

}
