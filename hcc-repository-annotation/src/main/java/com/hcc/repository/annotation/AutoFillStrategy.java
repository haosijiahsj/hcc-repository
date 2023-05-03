package com.hcc.repository.annotation;

/**
 * 自动填充插入策略
 *
 * @author hushengjun
 * @date 2023/4/21
 */
public interface AutoFillStrategy {

    /**
     * 是否自动填充
     * @return
     */
    default boolean autoFill(AutoFillContext context) {
        return true;
    }

    /**
     * 返回填充值
     * @return
     */
    default Object fill(AutoFillContext context) {
        throw new UnsupportedOperationException();
    }

}
