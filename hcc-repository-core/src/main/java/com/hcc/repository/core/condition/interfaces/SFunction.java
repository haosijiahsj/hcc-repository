package com.hcc.repository.core.condition.interfaces;

import java.io.Serializable;
import java.lang.invoke.SerializedLambda;
import java.lang.reflect.Method;
import java.util.function.Function;

/**
 * SFunction
 *
 * @author hushengjun
 * @date 2023/3/13
 */
@FunctionalInterface
public interface SFunction<T, R> extends Function<T, R>, Serializable {

    default SerializedLambda getSerializedLambda() throws Exception {
        // writeReplace改了好像会报异常
        Method write = this.getClass().getDeclaredMethod("writeReplace");
        write.setAccessible(true);
        return (SerializedLambda) write.invoke(this);
    }

    default String getImplClass() {
        try {
            return getSerializedLambda().getImplClass();
        } catch (Exception e) {
            return null;
        }
    }

    default String getImplMethodName() {
        try {
            return getSerializedLambda().getImplMethodName();
        } catch (Exception e) {
            return null;
        }
    }

}