package com.hcc.repository.test.dao.mapper;

import com.hcc.repository.core.annotation.Modifying;
import com.hcc.repository.core.annotation.Param;
import com.hcc.repository.core.annotation.Query;
import com.hcc.repository.core.conditions.ICondition;
import com.hcc.repository.core.mapper.BaseMapper;
import com.hcc.repository.core.page.IPage;
import com.hcc.repository.test.domain.po.ProductPo;

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
    @Query("update product p set p.name = :name")
    void updateSql(@Param("name") String name, ICondition<ProductPo> condition);

}
