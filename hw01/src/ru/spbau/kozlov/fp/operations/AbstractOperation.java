package ru.spbau.kozlov.fp.operations;

import ru.spbau.kozlov.fp.terms.ITerm;

/**
 * @author adkozlov
 */
public abstract class AbstractOperation implements IOperation {

    private final ITerm term;
    private ITerm result;

    public AbstractOperation(ITerm term) {
        this.term = term;
    }

    protected ITerm getTerm() {
        return term;
    }

    @Override
    public ITerm getResult() {
        if (result == null) {
            result = doOperation();
        }

        return result;
    }

    protected abstract ITerm doOperation();

    @Override
    public String toString() {
        return term + " ->" + getOperationSymbol() + " " + getResult();
    }
}
