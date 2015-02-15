package ru.spbau.kozlov.fp;

import ru.spbau.kozlov.fp.operations.Normalization;
import ru.spbau.kozlov.fp.terms.Application;
import ru.spbau.kozlov.fp.terms.ITerm;
import ru.spbau.kozlov.fp.terms.Lambda;
import ru.spbau.kozlov.fp.terms.Variable;

public class Main {

    private static final ITerm[] TERMS = new ITerm[]{
            new Application(new Variable("x"), new Application(new Lambda("x", new Variable("x")), new Variable("y"))),
            new Application(new Lambda("z", new Application(new Variable("z"), new Variable("z"))), new Application(new Lambda("x", new Variable("x")), new Variable("y"))),
            new Application(new Application(new Lambda("f", new Lambda("g", new Lambda("x", new Application(new Application(new Variable("f"), new Variable("x")), new Application(new Variable("g"), new Variable("x")))))), new Lambda("x", new Lambda("y", new Variable("x")))), new Lambda("x", new Lambda("y", new Variable("x"))))
    };

    public static void main(String[] args) {
        for (ITerm term : TERMS) {
            System.out.println(new Normalization(term));
            System.out.println();
        }
    }
}
