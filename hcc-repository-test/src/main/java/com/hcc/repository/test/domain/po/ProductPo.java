package com.hcc.repository.test.domain.po;

import com.hcc.repository.annotation.Table;

/**
 * ProductPo
 *
 * @author hushengjun
 * @date 2023/4/6
 */
@Table("product")
public class ProductPo extends BasePo {

    private Long id;
    private String name;

}
