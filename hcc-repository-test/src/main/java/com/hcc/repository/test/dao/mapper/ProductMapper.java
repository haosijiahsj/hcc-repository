package com.hcc.repository.test.dao.mapper;

import com.hcc.repository.core.annotation.Condition;
import com.hcc.repository.core.annotation.Modifying;
import com.hcc.repository.core.annotation.Param;
import com.hcc.repository.core.annotation.Query;
import com.hcc.repository.core.conditions.ICondition;
import com.hcc.repository.core.mapper.BaseMapper;
import com.hcc.repository.core.page.IPage;
import com.hcc.repository.test.domain.ProductQueryParam;
import com.hcc.repository.test.domain.po.ProductPo;

import java.util.List;

/**
 * ProductMapper
 *
 * @author hushengjun
 * @date 2023/4/6
 */
//@DS("dataSource1")
public interface ProductMapper extends BaseMapper<ProductPo, Long> {

    @Query("select * from product where id = :id")
    ProductPo query(@Param("id") int id);

    @Query("select * from product where id = :id")
    IPage<ProductPo> queryPage(@Param("id") int id, IPage pageParam);

    @Query("select * from product")
    IPage<ProductPo> queryPageCondition(ICondition condition, IPage pageParam);

    @Modifying
    @Query("update product p set p.name = #{name}")
    void updateSql(@Param("name") String name, ICondition<ProductPo> condition);

//    @Query("select * from product p")
//    @Conditions({
//            @Condition(exp = "#p.name != null", value = "AND p.name = #{p.name}"),
//            @Condition(exp = "#p.id != null", value = "AND p.id = #{p.id}"),
//            @Condition(exp = "#p.ids != null && #p.ids.size() > 0", value = "AND p.id IN (${p.ids})"),
//            @Condition(exp = "#p.name != null", value = "AND p.name like ${p.name}")
//    }
//    )
//    List<ProductPo> selectProducts1(@Param("p") ProductQueryParam p);

    @Query(value = "select * from product p",
            conditions = {
                    @Condition(exp = "#p.name != null", value = "AND p.name = #{p.name}"),
                    @Condition(exp = "#p.id != null", value = "AND p.id = #{p.id}"),
                    @Condition(exp = "#p.ids != null && #p.ids.size() > 0", value = "AND p.id IN (${p.ids})"),
                    @Condition(exp = "#p.name != null", value = "AND p.name like ${p.name}")
            })
    List<ProductPo> selectProducts(@Param("p") ProductQueryParam p);
}
