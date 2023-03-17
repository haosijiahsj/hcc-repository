package com.hcc.repository.core.condition;

import com.hcc.repository.core.constants.SqlKeywordEnum;

import java.util.ArrayList;
import java.util.List;

/**
 * SegmentContainer
 *
 * @author hushengjun
 * @date 2023/3/10
 */
public class SegmentContainer {

    private final List<String> whereSqlSegments;
    private final List<String> orderBySegments;
    private final List<String> groupBySegments;
    private final List<String> havingSegments;

    public SegmentContainer() {
        whereSqlSegments = new ArrayList<>(16);
        orderBySegments = new ArrayList<>(16);
        groupBySegments = new ArrayList<>(8);
        havingSegments = new ArrayList<>(8);
    }

    public void add(SqlKeywordEnum sqlKeyword, String sqlSegment) {

    }

}
