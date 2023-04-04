package com.hcc.repository.extension.transaction;

import org.springframework.transaction.PlatformTransactionManager;
import org.springframework.transaction.TransactionDefinition;
import org.springframework.transaction.TransactionStatus;
import org.springframework.transaction.support.DefaultTransactionDefinition;

/**
 * TransactionHelper
 *
 * @author hushengjun
 * @date 2023/4/5
 */
public class TransactionHelper {

    private final PlatformTransactionManager transactionManager;
    private TransactionDefinition definition;

    public TransactionHelper(PlatformTransactionManager transactionManager) {
        this.transactionManager = transactionManager;
    }

    public TransactionHelper(PlatformTransactionManager transactionManager, TransactionDefinition definition) {
        this.transactionManager = transactionManager;
        this.definition = definition;
    }

    /**
     * 事务操作
     * @param action
     */
    public <T> T doTransaction(Action<T> action) {
        TransactionDefinition definition = this.definition;
        if (definition == null) {
            definition = new DefaultTransactionDefinition();
        }
        TransactionStatus transactionStatus = transactionManager.getTransaction(definition);
        T result;
        try {
            result = action.doSomething(transactionStatus);
            transactionManager.commit(transactionStatus);
        } catch (Exception e) {
            transactionManager.rollback(transactionStatus);
            throw e;
        }

        return result;
    }

    @FunctionalInterface
    public interface Action<T> {
        T doSomething(TransactionStatus transactionStatus);
    }

}
