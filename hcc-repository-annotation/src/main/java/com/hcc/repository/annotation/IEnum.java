package com.hcc.repository.annotation;

import java.io.Serializable;

/**
 * 基础枚举接口
 *
 * @author hushengjun
 * @date 2023/4/5
 */
public interface IEnum<T extends Serializable> {

    /**
     * 获取值
     * @return
     */
    T getValue();

}
