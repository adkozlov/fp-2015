package ru.spbau.kozlov.fp.operations;

import ru.spbau.kozlov.fp.terms.ITerm;

import java.util.LinkedList;
import java.util.List;

/**
 * @author adkozlov
 */
public class Normalization extends AbstractOperation {

    private final List<ITerm> terms = new LinkedList<>();

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

    @Override
    public String toString() {
        getResult();

        StringBuilder result = new StringBuilder(getTerm().toString());
        for (ITerm term : terms) {
            result.append(String.format(OUTPUT_FORMAT, BetaReduction.BETA_STRING));
            result.append("\n\t");
            result.append(term);
        }

        return result.toString();
    }

    private ITerm normalize(ITerm term) {
        ITerm reduced = new BetaReduction(term).getResult();

        if (term.equals(reduced)) {
            return term;
        } else {
            terms.add(reduced);
            return normalize(reduced);
        }
    }
}
