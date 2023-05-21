package com.hcc.repository.test.domain;

import lombok.Data;

import java.util.List;

/**
 * Param
 *
 * @author hushengjun
 * @date 2023/5/20
 */
@Data
public class ProductQueryParam {

    private String name;
    private Long id;
    private List<Long> ids;

}
