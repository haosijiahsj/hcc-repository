package com.hcc.repository.extension.converter.codec;

import com.hcc.repository.core.utils.Assert;
import com.hcc.repository.core.utils.StrUtils;

import javax.crypto.Cipher;
import javax.crypto.spec.SecretKeySpec;
import java.nio.charset.StandardCharsets;
import java.util.Base64;

/**
 * AES加解密
 *
 * @author hushengjun
 * @date 2023/8/10
 */
public class AESEncryptConverter implements CodecConverter {

    /**
     * 加解密密钥
     * @return
     */
    protected String secretKey() {
        return "hcc-repository29";
    }

    /**
     * 解密失败回调方法
     * @param encryptedStr
     * @param e
     * @return
     */
    protected String onDecryptException(String encryptedStr, Exception e) {
        throw new RuntimeException(e);
    }

    private byte[] getSecretKey() {
        String secretKey = secretKey();
        Assert.isTrue(StrUtils.isNotEmpty(secretKey), "密钥不能为空");

        byte[] bytes = secretKey.getBytes(StandardCharsets.UTF_8);
        if (bytes.length * 8 != 128 && bytes.length * 8 != 192 && bytes.length * 8 != 256) {
            throw new IllegalArgumentException(StrUtils.format("密钥：[{0}]非法，只能是128、192、256位", secretKey));
        }

        return bytes;
    }

    /**
     * 加密
     * @param originalStr
     * @return
     */
    @Override
    public String encode(String originalStr) {
        SecretKeySpec keySpec = new SecretKeySpec(this.getSecretKey(), "AES");

        try {
            Cipher cipher = Cipher.getInstance("AES/ECB/PKCS5Padding");
            cipher.init(Cipher.ENCRYPT_MODE, keySpec);
            byte[] encryptedBytes = cipher.doFinal(originalStr.getBytes(StandardCharsets.UTF_8));

            return Base64.getEncoder().encodeToString(encryptedBytes);
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    /**
     * 解密
     * @param encodedStr
     * @return
     */
    @Override
    public String decode(String encodedStr) {
        SecretKeySpec keySpec = new SecretKeySpec(this.getSecretKey(), "AES");

        try {
            Cipher cipher = Cipher.getInstance("AES/ECB/PKCS5Padding");
            cipher.init(Cipher.DECRYPT_MODE, keySpec);
            byte[] encodedBytes = Base64.getDecoder().decode(encodedStr.getBytes(StandardCharsets.UTF_8));

            byte[] decryptedBytes = cipher.doFinal(encodedBytes);

            return new String(decryptedBytes);
        } catch (Exception e) {
            return onDecryptException(encodedStr, e);
        }
    }

}
