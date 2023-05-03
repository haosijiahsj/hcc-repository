package com.hcc.repository.test.domain.po;

import com.hcc.repository.annotation.AutoFillContext;
import com.hcc.repository.annotation.AutoFillStrategy;
import com.hcc.repository.annotation.Column;
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

    @Column(insertStrategy = TimeAutoFillStrategy.class)
    private LocalDateTime createTime;
    @Column(insertStrategy = TimeAutoFillStrategy.class, updateStrategy = TimeAutoFillStrategy.class)
    private LocalDateTime updateTime;
    @LogicDelete(value = "0", delValue = "1", logicDelValueType = LogicDelValueType.UUID)
    private String deleted;

    public static class TimeAutoFillStrategy implements AutoFillStrategy {
        @Override
        public Object fill(AutoFillContext context) {
            return LocalDateTime.now();
        }
    }

}
