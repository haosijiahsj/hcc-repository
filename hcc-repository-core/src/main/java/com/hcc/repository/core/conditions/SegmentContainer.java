package com.hcc.repository.core.conditions;

import com.hcc.repository.core.constants.SqlKeywordEnum;
import com.hcc.repository.core.constants.StrPool;
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

    /**
     * where语句
     */
    private final List<String> whereSqlSegments;
    /**
     * order by语句
     */
    private final List<String> orderBySegments;
    /**
     * group by语句
     */
    private final List<String> groupBySegments;
    /**
     * having语句
     */
    private final List<String> havingSegments;
    /**
     * 直接拼接的最后一句sql
     */
    private String lastSql;

    public SegmentContainer() {
        whereSqlSegments = new ArrayList<>(16);
        orderBySegments = new ArrayList<>(16);
        groupBySegments = new ArrayList<>(8);
        havingSegments = new ArrayList<>(8);
    }

    /**
     * 添加AND语句
     * @param sqlSegment
     */
    public void addAndSegment(String sqlSegment) {
        if (CollUtils.isEmpty(whereSqlSegments)) {
            // 当这个为空时，说明当前是第一个条件则不拼AND
            whereSqlSegments.add(sqlSegment);
        } else {
            String curLast = whereSqlSegments.get(whereSqlSegments.size() - 1);
            String sqlSegmentStr = SqlKeywordEnum.AND.getKeyword();
            if (curLast.equals(SqlKeywordEnum.OR.getKeyword())) {
                // 加入前的最后一个为OR则移除，同时本次拼接OR
                sqlSegmentStr = SqlKeywordEnum.OR.getKeyword();
                whereSqlSegments.remove(whereSqlSegments.size() - 1);
            } else if (curLast.equals(SqlKeywordEnum.AND.getKeyword())) {
                whereSqlSegments.remove(whereSqlSegments.size() - 1);
            }
            whereSqlSegments.add(sqlSegmentStr + StrPool.SPACE + sqlSegment);
        }
    }

    /**
     * 添加普通语句
     * @param sqlSegment
     */
    public void addPlainSegment(String sqlSegment) {
        whereSqlSegments.add(sqlSegment);
    }

    /**
     * 添加order by语句
     * @param sqlSegment
     */
    public void addOrderBySegment(String sqlSegment) {
        orderBySegments.add(sqlSegment);
    }

    /**
     * 添加group by语句
     * @param sqlSegment
     */
    public void addGroupBySegment(String sqlSegment) {
        groupBySegments.add(sqlSegment);
    }

    /**
     * 添加having语句
     * @param sqlSegment
     */
    public void addHavingSegment(String sqlSegment) {
        havingSegments.add(sqlSegment);
    }

    public void setLastSql(String lastSql) {
        this.lastSql = lastSql;
    }

    /**
     * where子句
     * @return
     */
    public String getSqlWhere() {
        List<String> whereSqlSegments = getWhereSqlSegments();
        if (CollUtils.isEmpty(whereSqlSegments)) {
            return StrPool.EMPTY;
        }
        String lastSqlSegment = whereSqlSegments.get(whereSqlSegments.size() - 1);
        if (SqlKeywordEnum.AND.getKeyword().equals(lastSqlSegment)
                || SqlKeywordEnum.OR.getKeyword().equals(lastSqlSegment)) {
            // 最后一个where sql片段为AND 或 OR，则去掉
            whereSqlSegments.remove(whereSqlSegments.size() - 1);
        }

        // 去掉第一个AND OR
        String firstCondition = whereSqlSegments.get(0);
        if (firstCondition.startsWith(SqlKeywordEnum.AND.getKeyword())
                || firstCondition.startsWith(SqlKeywordEnum.OR.getKeyword())) {
            if (firstCondition.startsWith(SqlKeywordEnum.AND.getKeyword())) {
                if (firstCondition.endsWith(StrPool.SPACE)) {
                    firstCondition = firstCondition.substring(4);
                } else {
                    firstCondition = firstCondition.substring(3);
                }
            } else if (firstCondition.startsWith(SqlKeywordEnum.OR.getKeyword())) {
                if (firstCondition.endsWith(StrPool.SPACE)) {
                    firstCondition = firstCondition.substring(3);
                } else {
                    firstCondition = firstCondition.substring(2);
                }
            }
            whereSqlSegments.remove(0);
            whereSqlSegments.add(0, firstCondition.trim());
        }

        return SqlKeywordEnum.WHERE.getKeyword() + StrPool.SPACE + String.join(StrPool.SPACE, whereSqlSegments);
    }

    /**
     * order by子句
     * @return
     */
    public String getSqlOrderBy() {
        List<String> orderBySegments = this.getOrderBySegments();
        if (CollUtils.isEmpty(orderBySegments)) {
            return StrPool.EMPTY;
        }

        return SqlKeywordEnum.ORDER_BY.getKeyword() + StrPool.SPACE + String.join(StrPool.COMMA_SPACE, orderBySegments);
    }

    /**
     * group by子句
     * @return
     */
    public String getSqlGroupBy() {
        List<String> groupBySegments = this.getGroupBySegments();
        if (CollUtils.isEmpty(groupBySegments)) {
            return StrPool.EMPTY;
        }

        return SqlKeywordEnum.GROUP_BY.getKeyword() + StrPool.SPACE + String.join(StrPool.COMMA_SPACE, groupBySegments);
    }

    /**
     * having子句
     * @return
     */
    public String getSqlHaving() {
        List<String> havingSegments = this.getHavingSegments();
        if (CollUtils.isEmpty(havingSegments)) {
            return StrPool.EMPTY;
        }

        return SqlKeywordEnum.HAVING.getKeyword() + StrPool.SPACE + String.join(StrPool.SPACE, havingSegments);
    }

    /**
     * where后的整个子句
     * @return
     */
    public String getSqlSegmentAfterWhere() {
        StringBuilder sb = new StringBuilder();
        String sqlWhere = this.getSqlWhere();
        if (StrUtils.isNotEmpty(sqlWhere)) {
            sb.append(sqlWhere);
        }
        String sqlGroupBy = this.getSqlGroupBy();
        if (StrUtils.isNotEmpty(sqlGroupBy)) {
            if (sb.length() > 0) {
                sb.append(StrPool.SPACE);
            }
            sb.append(sqlGroupBy);
        }
        String sqlHaving = this.getSqlHaving();
        if (StrUtils.isNotEmpty(sqlHaving)) {
            if (sb.length() > 0) {
                sb.append(StrPool.SPACE);
            }
            sb.append(sqlHaving);
        }
        String sqlOrderBy = this.getSqlOrderBy();
        if (StrUtils.isNotEmpty(sqlOrderBy)) {
            if (sb.length() > 0) {
                sb.append(StrPool.SPACE);
            }
            sb.append(sqlOrderBy);
        }
        if (StrUtils.isNotEmpty(lastSql)) {
            if (sb.length() > 0) {
                sb.append(StrPool.SPACE);
            }
            sb.append(lastSql);
        }

        return sb.toString();
    }

}
