package com.hcc.repository.extension.converter.mask;

/**
 * 用于临时跳过mask
 *
 * @author hushengjun
 * @date 2023/4/23
 */
public class MaskContextHolder {

    private static final ThreadLocal<Boolean> flag = new ThreadLocal<>();

    /**
     * 跳过mask
     */
    public static void skipMask() {
        flag.set(Boolean.TRUE);
    }

    /**
     * 恢复mask
     */
    public static void restoreMask() {
        flag.remove();
    }

    /**
     * 获取当前flag
     * @return
     */
    public static boolean get() {
        return flag.get() != null && flag.get();
    }

}
