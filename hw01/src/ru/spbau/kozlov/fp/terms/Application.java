package ru.spbau.kozlov.fp.terms;

import java.util.HashSet;
import java.util.Set;

/**
 * @author adkozlov
 */
public class Application extends AbstractTerm {

    private final AbstractTerm firstTerm;
    private final AbstractTerm secondTerm;

    public Application(ITerm firstTerm, ITerm secondTerm) {
        this.firstTerm = (AbstractTerm) firstTerm;
        this.secondTerm = (AbstractTerm) secondTerm;
    }

    @Override
    public String toString() {
        return "(" + firstTerm + " " + secondTerm + ")";
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof Application)) return false;

        Application that = (Application) o;
        return firstTerm.equals(that.firstTerm) && secondTerm.equals(that.secondTerm);

    }

    @Override
    public int hashCode() {
        int result = firstTerm.hashCode();
        result = 31 * result + secondTerm.hashCode();
        return result;
    }

    @Override
    public ITerm reduce() {
        if (firstTerm instanceof Lambda) {
            Lambda lambda = (Lambda) firstTerm;
            return lambda.getTerm().substitute(lambda.getParameter(), secondTerm.reduce());
        } else {
            return new Application(firstTerm.reduce(), secondTerm.reduce());
        }
    }

    @Override
    public ITerm substitute(String parameter, ITerm term) {
        return new Application(firstTerm.substitute(parameter, term), secondTerm.substitute(parameter, term));
    }

    @Override
    public Set<String> getVariables(Set<String> variables) {
        Set<String> result = new HashSet<>(firstTerm.getVariables(variables));
        result.addAll(secondTerm.getVariables(variables));

        return result;
    }
}

