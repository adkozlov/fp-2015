package ru.spbau.kozlov.fp;

import ru.spbau.kozlov.fp.operations.Normalization;
import ru.spbau.kozlov.fp.terms.Application;
import ru.spbau.kozlov.fp.terms.ITerm;
import ru.spbau.kozlov.fp.terms.Lambda;
import ru.spbau.kozlov.fp.terms.Variable;

public class Main {

    private static final ITerm[] TERMS = new ITerm[]{
            new Application(new Variable("x"), new Application(new Lambda("x", new Variable("x")), new Variable("y"))),
            new Application(new Lambda("z", new Application(new Variable("z"), new Variable("z"))), new Application(new Lambda("x", new Variable("x")), new Variable("y")))
    };

    public static void main(String[] args) {
        for (ITerm term : TERMS) {
            System.out.println(new Normalization(term));
        }
    }
}
