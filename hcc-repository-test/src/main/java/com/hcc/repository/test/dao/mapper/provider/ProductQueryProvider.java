package com.hcc.repository.test.dao.mapper.provider;


/**
 * ProductSelectProvider
 *
 * @author hushengjun
 * @date 2023/7/25
 */
public class ProductQueryProvider {

    public String selectProvider1(Long id) {
        return "SELECT * FROM product p WHERE id = #{id}";
    }

    public String updateProvider1(Long id) {
        return "update product p set p.price = 1 where id = #{id}";
    }

}
