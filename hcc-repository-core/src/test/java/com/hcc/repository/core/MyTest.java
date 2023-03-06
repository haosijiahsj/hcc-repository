package com.hcc.repository.core;

import com.hcc.repository.core.mapper.BaseMapper;
import com.hcc.repository.core.proxy.InjectMapperProxyFactory;
import org.springframework.jdbc.core.JdbcTemplate;

import java.lang.reflect.Field;
import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;
import java.lang.reflect.Proxy;

/**
 * MyTest
 *
 * @author hushengjun
 * @date 2023/3/6
 */
public class MyTest {

    public static void main(String[] args) {
        Class<MyMapper> clazz = MyMapper.class;
        System.out.println(MyTest.class.isAssignableFrom(clazz));
//        MyMapper mapper = InjectMapperProxyFactory.create(MyMapper.class, new JdbcTemplate());
//        mapper.insert(null);

        MyMapper mapper1 = (MyMapper) Proxy.newProxyInstance(clazz.getClassLoader(), new Class[]{clazz}, (proxy, method, args1) -> {
            System.out.println("代理");
            return null;
        });
        mapper1.insert(null);
    }

    public interface MyMapper extends BaseMapper<Object, Long> {}

}
