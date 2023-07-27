package com.hcc.repository.core.handler.annotation;

import com.hcc.repository.core.annotation.QueryProvider;
import com.hcc.repository.core.conditions.ICondition;
import com.hcc.repository.core.conditions.nativesql.NativeSqlCondition;
import com.hcc.repository.core.utils.Assert;
import com.hcc.repository.core.utils.ReflectUtils;
import com.hcc.repository.core.utils.StrUtils;
import lombok.extern.slf4j.Slf4j;

import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.Arrays;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * 查询注解处理器
 *
 * @author hushengjun
 * @date 2023/4/28
 */
@Slf4j
public class QueryProviderAnnotationMethodHandler extends QueryAnnotationMethodHandler {

    private final QueryProvider queryProviderAnnotation;
    private Class<?> providerClassType;
    private Method providerMethod;

    public QueryProviderAnnotationMethodHandler(QueryProvider queryProviderAnnotation) {
        super(queryProviderAnnotation.resultMapper());
        this.queryProviderAnnotation = queryProviderAnnotation;
    }

    @Override
    protected void prepare() {
        // Provider类名
        providerClassType = queryProviderAnnotation.value();
        if (providerClassType == null || void.class.equals(providerClassType)) {
            providerClassType = queryProviderAnnotation.type();
        }
        Assert.isFalse(providerClassType == null || void.class.equals(providerClassType), "需要指定value或type");
        Assert.isFalse(providerClassType.isInterface(), "provider class不能为interface");

        // Provider方法名，不指定则使用与mapper同名方法
        String providerMethodName = queryProviderAnnotation.method();
        if (StrUtils.isEmpty(providerMethodName)) {
            providerMethodName = method.getName();
        }

        // 查找对应的Provider方法
        providerMethod = ReflectUtils.getDeclaredMethod(providerClassType, providerMethodName, method.getParameterTypes());
        Assert.isNotNull(providerMethod, "未在类：{0}找到方法：{1}({2})", providerClassType.getName(), providerMethodName,
                Arrays.stream(method.getParameterTypes()).map(Class::getName).collect(Collectors.joining(", ")));

        // 判断返回值必须为String
        Class<?> returnType = providerMethod.getReturnType();
        Assert.isTrue(String.class.equals(returnType), "{0}.{1}方法返回值必须为String", providerClassType.getName(), providerMethodName);
    }

    @Override
    protected ICondition<?> prepareCondition() {
        boolean isStaticMethod = Modifier.isStatic(providerMethod.getModifiers());

        Object providerObject = null;
        if (!isStaticMethod) {
            // 非static方法需实例化Provider类调用
            providerObject = ReflectUtils.newInstance(providerClassType);
        }

        // 执行方法并获取到sql
        String sql = ReflectUtils.invokeMethod(providerObject, providerMethod, String.class, args);
        if (log.isDebugEnabled()) {
            log.debug("{}.{}({})执行结果为：{}", providerClassType.getName(), providerMethod.getName(),
                    Arrays.stream(method.getParameterTypes()).map(Class::getName).collect(Collectors.joining(", ")),
                    sql);
        }
        Assert.isTrue(StrUtils.isNotEmpty(sql), "{0}.{1}({2})生成的sql为空", providerClassType.getName(), providerMethod.getName(),
                Arrays.stream(method.getParameterTypes()).map(Class::getName).collect(Collectors.joining(", ")));

        // 构建Condition
        NativeSqlCondition<?> condition = new NativeSqlCondition<>();
        condition.sql(sql);
        condition.putParamMap(super.collectParam());

        return condition;
    }

}
