package com.hcc.repository.core.handler;

import com.hcc.repository.core.annotation.Modifying;
import com.hcc.repository.core.annotation.ModifyingProvider;
import com.hcc.repository.core.annotation.Query;
import com.hcc.repository.core.annotation.QueryProvider;
import com.hcc.repository.core.constants.MethodNameEnum;
import com.hcc.repository.core.handler.annotation.ModifyingAnnotationMethodHandler;
import com.hcc.repository.core.handler.annotation.ModifyingProviderAnnotationMethodHandler;
import com.hcc.repository.core.handler.annotation.QueryAnnotationMethodHandler;
import com.hcc.repository.core.handler.annotation.QueryProviderAnnotationMethodHandler;
import com.hcc.repository.core.utils.ReflectUtils;

import java.lang.reflect.Method;
import java.util.Arrays;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 * MethodHandlerFactory
 *
 * @author hushengjun
 * @date 2023/3/7
 */
public class MethodHandlerFactory {

    private static final Map<String, MethodNameEnum> methodNameEnumMap;

    static {
        methodNameEnumMap = Arrays.stream(MethodNameEnum.values())
                .collect(Collectors.toMap(MethodNameEnum::getMethodName, Function.identity()));
    }

    /**
     * 获取方法处理器
     * @param method
     * @return
     */
    public static AbstractMethodHandler create(Method method) {
        // 判断注解
        AbstractMethodHandler queryAnnotationMethodHandler = getQueryAnnotationMethodHandler(method);
        if (queryAnnotationMethodHandler != null) {
            return queryAnnotationMethodHandler;
        }
        AbstractMethodHandler queryProviderAnnotationMethodHandler = getQueryProviderAnnotationMethodHandler(method);
        if (queryProviderAnnotationMethodHandler != null) {
            return queryProviderAnnotationMethodHandler;
        }

        MethodNameEnum methodNameEnum = methodNameEnumMap.get(method.getName());
        if (methodNameEnum == null) {
            throw new IllegalArgumentException("不支持的方法");
        }

        return ReflectUtils.newInstance(methodNameEnum.getHandlerClass());
    }

    /**
     * Query注解
     * @param method
     * @return
     */
    private static AbstractMethodHandler getQueryAnnotationMethodHandler(Method method) {
        Query queryAnnotation = method.getAnnotation(Query.class);
        if (queryAnnotation != null) {
            Modifying modifyingAnnotation = method.getAnnotation(Modifying.class);
            if (modifyingAnnotation != null) {
                return new ModifyingAnnotationMethodHandler(queryAnnotation, modifyingAnnotation);
            }

            return new QueryAnnotationMethodHandler(queryAnnotation);
        }

        return null;
    }

    /**
     * QueryProvider注解
     * @param method
     * @return
     */
    private static AbstractMethodHandler getQueryProviderAnnotationMethodHandler(Method method) {
        QueryProvider queryProviderAnnotation = method.getAnnotation(QueryProvider.class);
        if (queryProviderAnnotation != null) {
            ModifyingProvider modifyingProviderAnnotation = method.getAnnotation(ModifyingProvider.class);
            if (modifyingProviderAnnotation != null) {
                return new ModifyingProviderAnnotationMethodHandler(queryProviderAnnotation, modifyingProviderAnnotation);
            }

            return new QueryProviderAnnotationMethodHandler(queryProviderAnnotation);
        }

        return null;
    }

}
