package com.hcc.repository.core.exceptions;

/**
 * TooManyResultException
 *
 * @author hushengjun
 * @date 2023/5/31
 */
public class TooManyResultException extends RepositoryException {

    public TooManyResultException() {
        super();
    }

    public TooManyResultException(String msg) {
        super(msg);
    }

    public TooManyResultException(Exception e) {
        super(e);
    }

    public TooManyResultException(String msg, Exception e) {
        super(msg, e);
    }

}
