package com.hcc.repository.extension.generator.id;

import com.hcc.repository.annotation.IdGenerator;
import com.hcc.repository.core.spring.config.RepositoryConfiguration;

import java.util.Optional;

/**
 * 雪花算法生成器
 *
 * @author hushengjun
 * @date 2023/4/14
 */
public class SnowFlakeIdGenerator implements IdGenerator<Long> {

    private static final String WORKER_ID = "workerId";
    private static final String DATACENTER_ID = "datacenterId";
    private static final String SNOW_FLAKE_BEGIN_TIMESTAMP = "snowflakeBeginTimestamp";
    private final RepositoryConfiguration configuration;

    private final SnowFlake snowFlake;

    // 框架会自动注入config值
    public SnowFlakeIdGenerator(RepositoryConfiguration configuration) {
        this.configuration = configuration;
        Long snowFlakeBeginTimestamp = Optional.ofNullable(this.getSnowFlakeBeginTimestamp()).orElse(this.getValue(this.getSnowFlakeBeginTimestampKey()));
        Long workerId = Optional.ofNullable(this.getWorkerId()).orElse(this.getValue(this.getWorkerIdKey()));
        Long datacenterId = Optional.ofNullable(this.getDatacenterId()).orElse(this.getValue(this.getDatacenterIdKey()));
        if (workerId == null) {
            throw new RuntimeException("workerId未指定");
        }
        if (datacenterId == null) {
            throw new RuntimeException("datacenterId未指定");
        }
        snowFlake = new SnowFlake(snowFlakeBeginTimestamp, workerId, datacenterId, 0L);
    }

    @Override
    public synchronized Long nextId() {
        return snowFlake.nextId();
    }

    protected String getSnowFlakeBeginTimestampKey() {
        return SNOW_FLAKE_BEGIN_TIMESTAMP;
    }

    protected String getWorkerIdKey() {
        return WORKER_ID;
    }

    protected String getDatacenterIdKey() {
        return DATACENTER_ID;
    }

    protected Long getSnowFlakeBeginTimestamp() {
        return null;
    }

    protected Long getWorkerId() {
        return null;
    }

    protected Long getDatacenterId() {
        return null;
    }

    /**
     * 先找配置文件，再找properties，再找环境变量
     * @return
     */
    protected Long getValue(String key) {
        Long value = null;
        Object obj = configuration.getProperties().get(key);
        if (obj != null) {
            value = Long.valueOf(obj.toString());
        }

        if (value == null) {
            Object propObj = System.getProperties().get(key);
            if (propObj != null) {
                value = Long.valueOf(propObj.toString());
            }
        }

        if (value == null) {
            String envVal = System.getenv().get(key);
            if (envVal != null) {
                value = Long.valueOf(envVal);
            }
        }

        return value;
    }

}
