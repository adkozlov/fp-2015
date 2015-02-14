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
        return reduce(getTerm());
    }

    @Override
    public String getOperationSymbol() {
        return "β";
    }

    private static ITerm reduce(ITerm term) {
        ITerm reduced = term.reduce();
        return term.equals(reduced) ? term : reduce(reduced);
    }
}
