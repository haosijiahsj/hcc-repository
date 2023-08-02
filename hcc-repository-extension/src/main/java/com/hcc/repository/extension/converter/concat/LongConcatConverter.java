package com.hcc.repository.extension.converter.concat;

import java.util.function.Function;

/**
 * LongConcatConverter
 *
 * @author hushengjun
 * @date 2023/8/2
 */
public class LongConcatConverter implements ConcatConverter<Long> {

    @Override
    public Function<String, Long> mapper() {
        return Long::parseLong;
    }

}
