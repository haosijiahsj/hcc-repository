package com.hcc.repository.core.handler;

import com.hcc.repository.core.constants.MethodNameEnum;

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

    public static AbstractMethodHandler create(String methodName) {
        MethodNameEnum methodNameEnum = methodNameEnumMap.get(methodName);
        if (methodNameEnum == null) {
            throw new IllegalArgumentException("不支持的方法");
        }

        try {
            return methodNameEnum.getHandlerClass().newInstance();
        } catch (InstantiationException | IllegalAccessException e) {
            throw new IllegalArgumentException("实例化处理类失败", e);
        }
    }

}
