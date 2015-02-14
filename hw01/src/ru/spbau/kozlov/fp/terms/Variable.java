package ru.spbau.kozlov.fp.terms;

import java.util.HashSet;
import java.util.Set;

/**
 * @author adkozlov
 */
public class Variable extends AbstractTerm {

    private final String name;

    public Variable(String name) {
        this.name = name;
    }

    @Override
    public String toString() {
        return name;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof Variable)) return false;

        Variable variable = (Variable) o;
        return name.equals(variable.name);

    }

    @Override
    public int hashCode() {
        return name.hashCode();
    }

    @Override
    public ITerm reduce() {
        return this;
    }

    @Override
    public ITerm substitute(String parameter, ITerm term) {
        return name.equals(parameter) ? term : this;
    }

    @Override
    public Set<String> getVariables(Set<String> variables) {
        Set<String> result = new HashSet<>();
        if (!variables.contains(name)) {
            result.add(name);
        }

        return result;
    }
}
