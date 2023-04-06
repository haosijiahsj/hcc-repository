package com.hcc.repository.test;

import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.Banner;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.builder.SpringApplicationBuilder;

/**
 * HccConfigCenterApplication
 *
 * @author hushengjun
 * @date 2022/10/6
 */
@Slf4j
@SpringBootApplication(scanBasePackages = "com.hcc.repository")
public class Application {

    public static void main(String[] args) {
        new SpringApplicationBuilder(Application.class)
                .bannerMode(Banner.Mode.OFF)
                .run(args);

        log.info("hcc-repository-test启动成功！");
    }

}
