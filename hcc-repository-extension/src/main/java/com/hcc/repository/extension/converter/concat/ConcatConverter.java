package com.hcc.repository.extension.converter.concat;

import com.hcc.repository.annotation.IConverter;
import com.hcc.repository.core.constants.StrPool;
import com.hcc.repository.core.utils.CollUtils;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 * 连接符转换器
 *
 * @author hushengjun
 * @date 2023/8/2
 */
@SuppressWarnings("unchecked")
public interface ConcatConverter<T> extends IConverter<List<T>, String> {

    default String getDelimiter() {
        return StrPool.COMMA;
    }

    @Override
    default String convertToColumn(List<T> attribute) {
        if (CollUtils.isEmpty(attribute)) {
            return null;
        }

        return attribute.stream()
                .filter(Objects::nonNull)
                .map(Object::toString)
                .collect(Collectors.joining(getDelimiter()));
    }

    @Override
    default List<T> convertToAttribute(String column) {
        if (column == null) {
            return Collections.emptyList();
        }

        return Arrays.stream(column.split(getDelimiter()))
                .map(mapper())
                .collect(Collectors.toList());
    }

    /**
     * 映射器
     * @return
     */
    default Function<String, T> mapper() {
        return s -> (T) s;
    }

}
