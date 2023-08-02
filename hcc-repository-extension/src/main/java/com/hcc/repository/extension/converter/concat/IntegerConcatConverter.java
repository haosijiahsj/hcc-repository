package com.hcc.repository.extension.converter.concat;

import java.util.function.Function;

/**
 * IntegerConcatConverter
 *
 * @author hushengjun
 * @date 2023/8/2
 */
public class IntegerConcatConverter implements ConcatConverter<Integer> {

    @Override
    public Function<String, Integer> mapper() {
        return Integer::parseInt;
    }

}
