package com.hcc.repository.extension.generator.id;

import com.hcc.repository.annotation.IdGenerator;
import com.hcc.repository.core.spring.config.RepositoryConfiguration;

/**
 * 雪花算法生成器
 *
 * @author hushengjun
 * @date 2023/4/14
 */
public class SnowFlakeIdGenerator implements IdGenerator<Long> {

    private static final String WORKER_ID = "WORKER_ID";
    private static final String DATACENTER_ID = "DATACENTER_ID";

    private final SnowFlake snowFlake;

    // 会自动注入
    public SnowFlakeIdGenerator(RepositoryConfiguration config) {
        Long workerId = null;
        Long datacenterId = null;
        if (config != null && config.getProperties() != null) {
            Object workerIdObj = config.getProperties().get(WORKER_ID);
            Object datacenterIdObj = config.getProperties().get(DATACENTER_ID);
            workerId = (workerIdObj == null ? null : Long.valueOf(workerIdObj.toString()));
            datacenterId = datacenterIdObj == null ? null : Long.valueOf(datacenterIdObj.toString());
        }
        if (workerId == null) {
            throw new RuntimeException("workerId未指定");
        }
        if (datacenterId == null) {
            throw new RuntimeException("datacenterId未指定");
        }
        snowFlake = new SnowFlake(workerId, datacenterId, 1L);
    }

    @Override
    public synchronized Long nextId() {
        return snowFlake.nextId();
    }

}
