package ru.spbau.kozlov.fp.operations;

import ru.spbau.kozlov.fp.terms.ITerm;

/**
 * @author adkozlov
 */
public interface IOperation {

    ITerm getResult();

    String getOperationSymbol();
}
