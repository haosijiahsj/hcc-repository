package com.hcc.repository.test.domain.po;

import lombok.Data;

import java.time.LocalDateTime;

/**
 * BasePo
 *
 * @author hushengjun
 * @date 2023/4/6
 */
@Data
public class BasePo {

    private LocalDateTime createTime;
    private LocalDateTime updateTime;

}
