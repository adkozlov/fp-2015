package ru.spbau.kozlov.fp.operations;

import ru.spbau.kozlov.fp.terms.ITerm;

/**
 * @author adkozlov
 */
public class Normalization extends AbstractOperation {

    public Normalization(ITerm term) {
        super(term);
    }

    @Override
    protected ITerm doOperation() {
        return normalize(getTerm());
    }

    @Override
    public String getOperationSymbol() {
        return "";
    }

    private static ITerm normalize(ITerm term) {
        ITerm reduced = new BetaReduction(term).getResult();
        return term.equals(reduced) ? term : normalize(reduced);
    }
}
