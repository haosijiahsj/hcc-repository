package com.hcc.repository.core.handler.select;

import com.hcc.repository.core.utils.CollUtils;

import java.util.List;
import java.util.Map;
import java.util.Optional;

/**
 * SelectOneHandler
 *
 * @author hushengjun
 * @date 2023/3/21
 */
public class SelectCountHandler extends AbstractSelectHandler {

    @Override
    protected void prepare() {}

    @Override
    protected Object executeSql(String sql, Object[] args) {
        List<Map<String, Object>> results = jdbcTemplateProxy.queryForList(sql, args);
        if (CollUtils.isEmpty(results)) {
            return null;
        }
        if (results.size() > 1) {
            throw new RuntimeException("结果不唯一");
        }
        Map<String, Object> result = results.get(0);
        if (result.size() > 1) {
            throw new RuntimeException("列值不唯一");
        }

        Optional<Object> optional = result.values().stream().findFirst();
        if (optional.isPresent()) {
            Object o = optional.get();
            return Long.valueOf(o.toString());
        }

        return null;
    }

}
