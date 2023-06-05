package com.hcc.repository.core.exceptions;

/**
 * IncorrectColumnCountException
 *
 * @author hushengjun
 * @date 2023/5/31
 */
public class IncorrectColumnCountException extends RepositoryException {

    private Integer exceptNum;
    private Integer actualNum;

    public IncorrectColumnCountException() {
        super();
    }

    public IncorrectColumnCountException(String msg) {
        super(msg);
    }

    public IncorrectColumnCountException(Integer exceptNum, Integer actualNum) {
        super(String.format("预期%s列，实际%s列", exceptNum, actualNum));
    }

    public IncorrectColumnCountException(Exception e) {
        super(e);
    }

    public IncorrectColumnCountException(String msg, Exception e) {
        super(msg, e);
    }

}
