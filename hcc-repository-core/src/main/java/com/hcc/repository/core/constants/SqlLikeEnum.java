package com.hcc.repository.core.constants;

/**
 * SqlLikeEnum
 *
 * @author hushengjun
 * @date 2023/3/24
 */
public enum SqlLikeEnum {

    LIKE,
    LIKE_LEFT,
    LIKE_RIGHT;

    public String getLikeVal(Object val) {
        if (val == null) {
            return "";
        }
        switch (this) {
            case LIKE: return "%" + val + "%";
            case LIKE_LEFT: return "%" + val;
            case LIKE_RIGHT: return val + "%";
            default: return "";
        }
    }

}
