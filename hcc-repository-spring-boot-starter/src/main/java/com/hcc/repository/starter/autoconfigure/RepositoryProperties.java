package com.hcc.repository.starter.autoconfigure;

import lombok.Data;

import java.util.List;

/**
 * RepositoryProperties
 *
 * @author hushengjun
 * @date 2023/4/5
 */
@Data
public class RepositoryProperties {

    private String banner;
    private List<String> entityPackages;

}
