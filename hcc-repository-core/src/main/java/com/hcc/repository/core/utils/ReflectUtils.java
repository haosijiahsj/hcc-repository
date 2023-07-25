package com.hcc.repository.core.utils;


import java.lang.annotation.Annotation;
import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationHandler;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Proxy;
import java.lang.reflect.Type;
import java.lang.reflect.TypeVariable;
import java.lang.reflect.WildcardType;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.ConcurrentHashMap;

/**
 *
 * @author 胡胜钧
 * @date 8/7 0007
 */
public class ReflectUtils {

    private static final Map<Class<?>, Object> INSTANCE_CACHE = new ConcurrentHashMap<>(64);

    private ReflectUtils() {}

    /**
     * 代理接口
     * @param interfaceType
     * @param handler
     * @param <T>
     * @return
     */
    public static <T> T newProxy(Class<T> interfaceType, InvocationHandler handler) {
        if (!interfaceType.isInterface()) {
            throw new IllegalArgumentException(String.format("[%s] is't a interface !", interfaceType));
        }

        Object object = Proxy.newProxyInstance(interfaceType.getClassLoader(), new Class<?>[] { interfaceType }, handler);

        return interfaceType.cast(object);
    }

    /**
     * 反射设置值
     * @param obj
     * @param field
     * @param value
     * @throws IllegalAccessException
     */
    public static void setValue(Object obj, Field field, Object value) {
        boolean accessible = field.isAccessible();
        if (!accessible) {
            field.setAccessible(true);
        }
        try {
            field.set(obj, value);
        } catch (IllegalAccessException e) {
            throw new RuntimeException(e);
        } finally {
            field.setAccessible(accessible);
        }
    }

    public static Object getValue(Object obj, Field field) {
        boolean accessible = field.isAccessible();
        if (!accessible) {
            field.setAccessible(true);
        }
        try {
            return field.get(obj);
        } catch (IllegalAccessException e) {
            throw new RuntimeException(e);
        } finally {
            field.setAccessible(accessible);
        }
    }

    /**
     * 反射调用方法
     * @param obj
     * @param method
     * @param args
     * @throws InvocationTargetException
     * @throws IllegalAccessException
     */
    public static Object invokeMethod(Object obj, Method method, Object...args) {
        return invokeMethod(obj, method, Object.class, args);
    }

    /**
     * 反射调用方法
     * @param obj
     * @param method
     * @param returnClass
     * @param args
     * @param <T>
     * @return
     * @throws InvocationTargetException
     * @throws IllegalAccessException
     */
    public static <T> T invokeMethod(Object obj, Method method, Class<T> returnClass, Object... args) {
        boolean accessible = method.isAccessible();
        if (!accessible) {
            method.setAccessible(true);
        }
        Object returnObj = null;
        try {
            returnObj = method.invoke(obj, args);
        } catch (IllegalAccessException | InvocationTargetException e) {
            throw new RuntimeException(e);
        }
        method.setAccessible(accessible);

        return returnClass.cast(returnObj);
    }

    /**
     * 获取方法
     * @param clazz
     * @param name
     * @param types
     * @return
     */
    public static Method getDeclaredMethod(Class<?> clazz, String name, Class<?>[] types) {
        try {
            return clazz.getDeclaredMethod(name, types);
        } catch (NoSuchMethodException e) {
            throw new RuntimeException(e);
        }
    }

    /**
     * 是否存在某个注解
     * @param clazz
     * @param annotationClass
     * @return
     */
    public static boolean hasAnnotation(Class<?> clazz, Class<? extends Annotation> annotationClass) {
        return clazz.getAnnotation(annotationClass) != null;
    }

    /**
     * 从字段中获取注解
     * @param field
     * @param annotationClass
     * @param <T>
     * @return
     */
    public static <T extends Annotation> T getAnnotation(Field field, Class<T> annotationClass) {
        if (field == null || annotationClass == null) {
            return null;
        }
        return field.getAnnotation(annotationClass);
    }

    /**
     * 从方法中获取注解
     * @param method
     * @param annotationClass
     * @param <T>
     * @return
     */
    public static <T extends Annotation> T getAnnotation(Method method, Class<T> annotationClass) {
        if (method == null || annotationClass == null) {
            return null;
        }
        return method.getAnnotation(annotationClass);
    }

    public static <T extends Annotation> T getAnnotation(Class<?> clazz, Class<T> annotationClass) {
        if (clazz == null || annotationClass == null) {
            return null;
        }
        return clazz.getAnnotation(annotationClass);
    }

    /**
     * 获取字段的真实泛型
     * @param field
     * @return
     */
    public static Class<?>[] getGenericClasses(Field field) {
        Type genericType = field.getGenericType();
        List<Class<?>> classes = new ArrayList<>();
        if (genericType instanceof ParameterizedType) {
            Type[] actualTypeArguments = ((ParameterizedType) genericType).getActualTypeArguments();
            for (Type type : actualTypeArguments) {
                classes.add((Class<?>) type);
            }
        }

        return classes.toArray(new Class[0]);
    }

    /**
     * 获取interface上的泛型类型
     * @param clazz
     * @return
     */
    public static Type[] getGenericClassesForInterface(Class<?> clazz) {
        Type[] types = clazz.getGenericInterfaces();
        ParameterizedType target = null;
        for (Type type : types) {
            if (type instanceof ParameterizedType) {
                Type[] typeArray = ((ParameterizedType) type).getActualTypeArguments();
                if (ArrayUtils.isNotEmpty(typeArray)) {
                    for (Type t : typeArray) {
                        if (t instanceof TypeVariable || t instanceof WildcardType) {
                            break;
                        } else {
                            target = (ParameterizedType) type;
                            break;
                        }
                    }
                }
                break;
            }
        }

        return target == null ? null : target.getActualTypeArguments();
    }

    /**
     * 实例化class
     * @param clazz
     * @param <T>
     * @return
     */
    public static <T> T newInstance(Class<T> clazz) {
        try {
            return clazz.newInstance();
        } catch (InstantiationException | IllegalAccessException e) {
            throw new RuntimeException(e);
        }
    }

    /**
     * 实例化class，带缓存
     * @param clazz
     * @param <T>
     * @return
     */
    public static <T> T newInstanceForCache(Class<T> clazz) {
        Object instance = INSTANCE_CACHE.get(clazz);
        if (instance != null) {
            return (T) instance;
        }
        T newInstance = newInstance(clazz);
        INSTANCE_CACHE.put(clazz, newInstance);

        return newInstance;
    }

    /**
     * 加载class
     * @param className
     * @return
     */
    public static Class<?> forName(String className) {
        try {
            return Class.forName(className);
        } catch (ClassNotFoundException e) {
            throw new RuntimeException(e);
        }
    }

    /**
     * 获取一个类所有字段，含父类
     * @param clazz
     * @return
     */
    public static List<Field> getAllDeclaredFields(Class<?> clazz) {
        if (clazz == null) {
            throw new NullPointerException();
        }

        List<Field> fields = new ArrayList<>();
        while (clazz != null) {
            Field[] declaredFields = clazz.getDeclaredFields();
            if (declaredFields.length > 0) {
                fields.addAll(Arrays.asList(declaredFields));
            }
            clazz = clazz.getSuperclass();
        }

        return fields;
    }

    public static Constructor<?> matchConstruct(Class<?> clazz, Class<?>...paramTypes) {
        Constructor<?>[] constructors = clazz.getDeclaredConstructors();
        Assert.isTrue(constructors.length >= 1, String.format("%s 无构造方法", clazz.getName()));

        int exceptParamCount = Optional.ofNullable(paramTypes).map(p -> p.length).orElse(0);
        for (Constructor<?> constructor : constructors) {
            int parameterCount = constructor.getParameterCount();
            Class<?>[] parameterTypes = constructor.getParameterTypes();
            if (exceptParamCount == 0 && parameterCount == 0) {
                return constructor;
            }
            if (parameterCount == exceptParamCount) {
                boolean match = true;
                for (int i = 0; i < parameterTypes.length; i++) {
                    if (!parameterTypes[i].equals(paramTypes[i])) {
                        match = false;
                        break;
                    }
                }
                if (match) {
                    return constructor;
                }
            }
        }

        return null;
    }

}
