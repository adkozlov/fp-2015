package ru.spbau.kozlov.fp.terms;

import java.util.Set;

/**
 * @author adkozlov
 */
public interface ITerm {

    ITerm reduce();

    ITerm substitute(String parameter, ITerm term);

    Set<String> getVariables();
}
