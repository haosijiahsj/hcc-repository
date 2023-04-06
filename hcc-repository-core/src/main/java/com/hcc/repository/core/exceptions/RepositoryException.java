package com.hcc.repository.core.exceptions;

/**
 * RepositoryException
 *
 * @author hushengjun
 * @date 2023/4/2
 */
public class RepositoryException extends RuntimeException {

    public RepositoryException() {
        super();
    }

    public RepositoryException(String msg) {
        super(msg);
    }

    public RepositoryException(Exception e) {
        super(e);
    }

    public RepositoryException(String msg, Exception e) {
        super(msg, e);
    }

}
