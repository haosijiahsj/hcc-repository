package com.hcc.repository.core.conditions;

import com.hcc.repository.core.utils.CollUtils;
import com.hcc.repository.core.utils.StrUtils;
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

    public String getSqlWhere() {
        List<String> whereSqlSegments = getWhereSqlSegments();
        if (CollUtils.isEmpty(whereSqlSegments)) {
            return "";
        }
        String firstWhere = whereSqlSegments.get(0);
        if (firstWhere.startsWith("AND") || firstWhere.startsWith("OR")) {
            if (firstWhere.startsWith("AND")) {
                firstWhere = firstWhere.substring(4);
            } else if (firstWhere.startsWith("OR")) {
                firstWhere = firstWhere.substring(3);
            }
            whereSqlSegments.remove(0);
            whereSqlSegments.add(0, firstWhere);
        }

        return "WHERE " + String.join(" ", whereSqlSegments);
    }

    public String getSqlOrderBy() {
        List<String> orderBySegments = getOrderBySegments();
        if (CollUtils.isEmpty(orderBySegments)) {
            return "";
        }
        return "ORDER BY " + String.join(", ", orderBySegments);
    }

    public String getSqlGroupBy() {
        List<String> groupBySegments = getGroupBySegments();
        if (CollUtils.isEmpty(groupBySegments)) {
            return "";
        }
        return "GROUP BY " + String.join(", ", groupBySegments);
    }

    public String getSqlHaving() {
        List<String> havingSegments = getHavingSegments();
        if (CollUtils.isEmpty(havingSegments)) {
            return "";
        }
        return "HAVING " + String.join(" ", havingSegments);
    }

    public String getSqlSegment() {
        StringBuilder sb = new StringBuilder();
        String sqlWhere = getSqlWhere();
        if (StrUtils.isNotEmpty(sqlWhere)) {
            sb.append(sqlWhere);
        }
        String sqlGroupBy = getSqlGroupBy();
        if (StrUtils.isNotEmpty(sqlGroupBy)) {
            sb.append(" ").append(sqlGroupBy);
        }
        String sqlHaving = getSqlHaving();
        if (StrUtils.isNotEmpty(sqlHaving)) {
            sb.append(" ").append(sqlHaving);
        }
        String sqlOrderBy = getSqlOrderBy();
        if (StrUtils.isNotEmpty(sqlOrderBy)) {
            sb.append(" ").append(sqlOrderBy);
        }

        return sb.toString();
    }

}
