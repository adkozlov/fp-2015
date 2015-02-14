package ru.spbau.kozlov.fp.terms;

import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

/**
 * @author adkozlov
 */
public abstract class AbstractTerm implements ITerm {

    @Override
    public Set<String> getVariables() {
        return Collections.unmodifiableSet(getVariables(new HashSet<String>()));
    }

    protected abstract Set<String> getVariables(Set<String> variables);
}
