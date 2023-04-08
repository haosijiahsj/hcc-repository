package com.hcc.repository.test.domain.po;

import com.hcc.repository.annotation.LogicDelValueType;
import com.hcc.repository.annotation.LogicDelete;
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
    @LogicDelete(value = "0", delValue = "1", logicDelValueType = LogicDelValueType.UUID)
    private String deleted;

}
