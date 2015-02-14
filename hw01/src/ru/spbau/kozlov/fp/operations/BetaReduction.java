package ru.spbau.kozlov.fp.operations;

import ru.spbau.kozlov.fp.terms.ITerm;

/**
 * @author adkozlov
 */
public class BetaReduction extends AbstractOperation {

    public BetaReduction(ITerm term) {
        super(term);
    }

    @Override
    public ITerm doOperation() {
        return getTerm().reduce();
    }

    @Override
    public String getOperationSymbol() {
        return "β";
    }
}
