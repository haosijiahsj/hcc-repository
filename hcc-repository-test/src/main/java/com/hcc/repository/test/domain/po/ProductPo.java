package com.hcc.repository.test.domain.po;

import com.hcc.repository.annotation.Id;
import com.hcc.repository.annotation.IdType;
import com.hcc.repository.annotation.PropSetListener;
import com.hcc.repository.annotation.Table;
import com.hcc.repository.extension.generator.id.SnowFlakeIdGenerator;
import com.hcc.repository.test.domain.enums.ProductStatusEnum;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.ToString;

import java.math.BigDecimal;

/**
 * ProductPo
 *
 * @author hushengjun
 * @date 2023/4/6
 */
@EqualsAndHashCode(callSuper = true)
@Data
@ToString(callSuper = true)
@Table(value = "product")
public class ProductPo extends BasePo {

    @Id(idType = IdType.GENERATE, generator = SnowFlakeIdGenerator.class)
    private Long id;
    private String name;
    private BigDecimal price;
    private ProductStatusEnum productStatus;

    public static class TestPropSetListener implements PropSetListener {
        @Override
        public Object onPropSet(Object entity, Object value, String propName, String columnName) {
            return value;
        }
    }

}
