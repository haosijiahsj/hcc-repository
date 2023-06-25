package com.hcc.repository.core.utils;

import org.springframework.core.DefaultParameterNameDiscoverer;
import org.springframework.core.ParameterNameDiscoverer;

import java.lang.reflect.Method;

/**
 * MethodParamNameUtils
 *
 * @author hushengjun
 * @date 2023/6/21
 */
public class MethodParamNameUtils {

    private static final ParameterNameDiscoverer parameterNameDiscoverer = new DefaultParameterNameDiscoverer();

    private MethodParamNameUtils() {}

    /**
     * 获取方法参数名称，支持带-parameter启动的，或类上的
     * @param method
     * @return
     */
    public static String[] getMethodParamNames(Method method) {
        return parameterNameDiscoverer.getParameterNames(method);
    }

}
