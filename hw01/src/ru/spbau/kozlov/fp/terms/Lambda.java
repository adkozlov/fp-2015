package ru.spbau.kozlov.fp.terms;

import java.util.HashSet;
import java.util.Set;

/**
 * @author adkozlov
 */
public class Lambda extends AbstractTerm {

    private final String parameter;
    private final AbstractTerm term;

    public Lambda(String parameter, ITerm term) {
        this.parameter = parameter;
        this.term = (AbstractTerm) term;
    }

    String getParameter() {
        return parameter;
    }

    ITerm getTerm() {
        return term;
    }

    @Override
    public String toString() {
        return "(λ" + parameter + " . " + term + ")";
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof Lambda)) return false;

        Lambda lambda = (Lambda) o;
        return parameter.equals(lambda.parameter) && term.equals(lambda.term);

    }

    @Override
    public int hashCode() {
        int result = parameter.hashCode();
        result = 31 * result + term.hashCode();
        return result;
    }

    @Override
    public ITerm reduce() {
        return new Lambda(parameter, term.reduce());
    }

    @Override
    public ITerm substitute(String parameter, ITerm term) {
        if (this.parameter.equals(parameter)) {
            return term;
        } else if (term.getVariables().contains(this.parameter)) {
            return new Lambda(createNewParameter(), createNewTerm()).substitute(parameter, term);
        } else {
            return new Lambda(this.parameter, this.term.substitute(parameter, term));
        }
    }

    @Override
    public Set<String> getVariables(Set<String> variables) {
        Set<String> newVariables = new HashSet<>(variables);
        newVariables.add(parameter);

        return term.getVariables(newVariables);
    }

    private String createNewParameter() {
        return parameter + '\'';
    }

    private ITerm createNewTerm() {
        return term.substitute(parameter, new Variable(createNewParameter()));
    }
}
