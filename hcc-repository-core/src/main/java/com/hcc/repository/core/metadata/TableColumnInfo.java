package com.hcc.repository.core.metadata;

import com.hcc.repository.annotation.AutoFillStrategy;
import com.hcc.repository.annotation.Constants;
import com.hcc.repository.annotation.IConverter;
import com.hcc.repository.annotation.IEnum;
import com.hcc.repository.annotation.IdGenerator;
import com.hcc.repository.annotation.IdType;
import com.hcc.repository.annotation.LogicDelValueType;
import lombok.Data;

import java.lang.reflect.Field;

/**
 * 实体列元数据
 *
 * @author hushengjun
 * @date 2023/3/6
 */
@Data
public class TableColumnInfo {

    /**
     * 列名
     */
    private String columnName;
    /**
     * 字段名
     */
    private String fieldName;
    /**
     * 字段
     */
    private Field field;

    // id相关信息
    /**
     * 是否主键
     */
    private boolean primaryKey = false;
    /**
     * 主键生成类型
     */
    private IdType idType;
    /**
     * 主键生成器
     */
    private Class<? extends IdGenerator> generator;
    /**
     * 转换器
     */
    private Class<? extends IConverter> converter = Constants.UnknownConverter.class;
    /**
     * 主键生成器是否使用单例
     */
    private boolean useSingletonIdGenerator = true;

    // 逻辑删除
    /**
     * 是否逻辑删除
     */
    private boolean logicDelete = false;
    /**
     * 逻辑删除值
     */
    private String logicDelVal;
    /**
     * 逻辑未删除值
     */
    private String logicNotDelVal;
    /**
     * 逻辑删除值类型
     */
    private LogicDelValueType logicDelValueType;

    // 乐观锁
    /**
     * 是否乐观锁字段
     */
    private boolean version = false;

    // 填充策略
    /**
     * 插入填充策略
     */
    private Class<? extends AutoFillStrategy> insertStrategy = Constants.UnknownFillStrategy.class;
    /**
     * 更新填充策略
     */
    private Class<? extends AutoFillStrategy> updateStrategy = Constants.UnknownFillStrategy.class;

    /**
     * 是否需要转换器处理
     * @return
     */
    public boolean needConvert() {
        return !Constants.UnknownConverter.class.equals(converter) && !IConverter.class.equals(converter);
    }

    /**
     * 是否需要插入时自动填充处理
     * @return
     */
    public boolean needAutoFillInsert() {
        return !Constants.UnknownFillStrategy.class.equals(insertStrategy) && !AutoFillStrategy.class.equals(insertStrategy);
    }

    /**
     * 是否需要更新时自动填充处理
     * @return
     */
    public boolean needAutoFillUpdate() {
        return !Constants.UnknownFillStrategy.class.equals(updateStrategy) && !AutoFillStrategy.class.equals(updateStrategy);
    }

    /**
     * 获取字段类型
     * @return
     */
    public Class<?> getFieldType() {
        return field.getType();
    }

    /**
     * 是否是枚举
     * @return
     */
    public boolean isEnum() {
        return field.getType().isEnum();
    }

    /**
     * 是否是IEnum枚举
     * @return
     */
    public boolean isIEnum() {
        return isEnum() && IEnum.class.isAssignableFrom(getFieldType());
    }

}
