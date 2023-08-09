package com.hcc.repository.test.domain.po;

import com.hcc.repository.annotation.Column;
import com.hcc.repository.annotation.Id;
import com.hcc.repository.annotation.IdType;
import com.hcc.repository.annotation.Table;
import com.hcc.repository.annotation.Version;
import com.hcc.repository.extension.generator.id.SnowFlakeIdGenerator;
import com.hcc.repository.test.domain.enums.ProductStatusEnum;
import com.hcc.repository.test.domain.listener.ProductStatusNamePropSetListener;
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
@Table(value = "product", propSet = ProductStatusNamePropSetListener.class)
public class ProductPo extends BasePo {

    @Id(idType = IdType.GENERATE, generator = SnowFlakeIdGenerator.class)
    private Long id;
    private String name;
    private BigDecimal price;
    private ProductStatusEnum productStatus;
    @Version
    private Integer version;
    @Column(ignore = true)
    private String productStatusName;

}
