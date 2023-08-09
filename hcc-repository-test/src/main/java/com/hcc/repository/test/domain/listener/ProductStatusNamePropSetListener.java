package com.hcc.repository.test.domain.listener;

import com.hcc.repository.annotation.DictPropSetListener;
import com.hcc.repository.test.domain.enums.ProductStatusEnum;
import com.hcc.repository.test.domain.po.ProductPo;

/**
 * ProductStatusNamePropSetListener
 *
 * @author hushengjun
 * @date 2023/8/9
 */
public class ProductStatusNamePropSetListener implements DictPropSetListener<ProductPo, ProductStatusEnum> {

    @Override
    public boolean test(Object entity, Object value, String propName, String columnName) {
        if (!ProductPo.class.equals(entity.getClass())) {
            return false;
        }
        if (!ProductStatusEnum.class.equals(value.getClass())) {
            return false;
        }

        return true;
    }

    @Override
    public String getDictFieldName() {
        return "productStatusName";
    }

    @Override
    public Object getDictValue(ProductPo entity, String columName, ProductStatusEnum value) {
        return value.getName();
    }

}
