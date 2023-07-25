package com.hcc.repository.test.dao.mapper.provider;

/**
 * ProductSelectProvider
 *
 * @author hushengjun
 * @date 2023/7/25
 */
public class ProductSelectProvider {

    public String selectProvider1(Long id) {
        return "SELECT * FROM product p WHERE id = #{id}";
    }

}
