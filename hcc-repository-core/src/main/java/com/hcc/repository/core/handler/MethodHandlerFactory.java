package com.hcc.repository.core.handler;

import com.hcc.repository.core.annotation.Modifying;
import com.hcc.repository.core.annotation.Query;
import com.hcc.repository.core.constants.MethodNameEnum;
import com.hcc.repository.core.handler.annotation.ModifyingAnnotationMethodHandler;
import com.hcc.repository.core.handler.annotation.QueryAnnotationMethodHandler;
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

    public static AbstractMethodHandler create(Method method) {
        // 判断注解
        Query queryAnnotation = method.getAnnotation(Query.class);
        if (queryAnnotation != null) {
            AbstractMethodHandler methodHandler = new QueryAnnotationMethodHandler(queryAnnotation);
            Modifying modifyingAnnotation = method.getAnnotation(Modifying.class);
            if (modifyingAnnotation != null) {
                methodHandler = new ModifyingAnnotationMethodHandler(queryAnnotation, modifyingAnnotation);
            }
            return methodHandler;
        }

        MethodNameEnum methodNameEnum = methodNameEnumMap.get(method.getName());
        if (methodNameEnum == null) {
            throw new IllegalArgumentException("不支持的方法");
        }

        return ReflectUtils.newInstance(methodNameEnum.getHandlerClass());
    }

}
