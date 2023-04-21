package com.hcc.repository.annotation;

/**
 * UnknownFillStrategy
 *
 * @author hushengjun
 * @date 2023/4/21
 */
public class UnknownFillStrategy implements AutoFillStrategy {

    @Override
    public boolean autoFill(AutoFillContext context) {
        return false;
    }

    @Override
    public Object fill(AutoFillContext context) {
        throw new UnsupportedOperationException();
    }

}
