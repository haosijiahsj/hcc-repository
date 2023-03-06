package com.hcc.repository.core.handler;

/**
 * 插入方法处理器
 *
 * @author hushengjun
 * @date 2023/3/6
 */
public class InsertMethodHandler extends AbstractMethodHandler {

    private static final String INSERT = "INSERT INTO `%s` (%s) VALUES (%s)";

    @Override
    public Object handleMethod() throws Exception {
        Object firstArg = args[0];
        if (firstArg == null) {
            throw new IllegalArgumentException("插入参数不能为空！");
        }



        return null;
    }

}
