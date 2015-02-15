package ru.spbau.kozlov.fp.operations;

import ru.spbau.kozlov.fp.terms.ITerm;

/**
 * @author adkozlov
 */
public class BetaReduction extends AbstractOperation {

    public static final String BETA_STRING = "β";

    public BetaReduction(ITerm term) {
        super(term);
    }

    @Override
    public ITerm doOperation() {
        return getTerm().reduce();
    }

    @Override
    public String getOperationSymbol() {
        return BETA_STRING;
    }
}
