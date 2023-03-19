package com.hcc.repository.core.condition;

import lombok.Getter;

import java.util.ArrayList;
import java.util.List;

/**
 * SegmentContainer
 *
 * @author hushengjun
 * @date 2023/3/10
 */
@Getter
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

    public void addAndSegment(String sqlSegment) {
        whereSqlSegments.add("AND " + sqlSegment);
    }

    public void addPlainSegment(String sqlSegment) {
        whereSqlSegments.add(sqlSegment);
    }

    public void addOrderBySegment(String sqlSegment) {
        orderBySegments.add(sqlSegment);
    }

    public void addGroupBySegment(String sqlSegment) {
        groupBySegments.add(sqlSegment);
    }

    public void addHavingSegment(String sqlSegment) {
        havingSegments.add(sqlSegment);
    }

}
