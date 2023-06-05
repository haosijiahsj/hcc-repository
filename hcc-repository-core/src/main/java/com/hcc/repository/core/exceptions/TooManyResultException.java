package com.hcc.repository.core.exceptions;

/**
 * TooManyResultException
 *
 * @author hushengjun
 * @date 2023/5/31
 */
public class TooManyResultException extends RepositoryException {

    private Integer exceptNum;
    private Integer actualNum;

    public TooManyResultException() {
        super();
    }

    public TooManyResultException(String msg) {
        super(msg);
    }

    public TooManyResultException(Integer exceptNum, Integer actualNum) {
        super(String.format("预期%s条数据，实际%s条数据", exceptNum, actualNum));
    }

    public TooManyResultException(Exception e) {
        super(e);
    }

    public TooManyResultException(String msg, Exception e) {
        super(msg, e);
    }

}
