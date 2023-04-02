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

}
