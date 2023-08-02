package com.hcc.repository.extension.converter.concat;

import java.util.function.Function;

/**
 * StringConcatConverter
 *
 * @author hushengjun
 * @date 2023/8/2
 */
public class StringConcatConverter implements ConcatConverter<String> {

    @Override
    public Function<String, String> mapper() {
        return Function.identity();
    }

}
