package meta;

import jason.JasonException;
import jason.asSemantics.DefaultInternalAction;
import jason.asSemantics.Option;
import jason.asSemantics.TransitionSystem;
import jason.asSemantics.Unifier;
import jason.asSyntax.ASSyntax;
import jason.asSyntax.Atom;
import jason.asSyntax.ListTerm;
import jason.asSyntax.ListTermImpl;
import jason.asSyntax.Plan;
import jason.asSyntax.PlanBody;
import jason.asSyntax.Pred;
import jason.asSyntax.Structure;
import jason.asSyntax.Term;
import jason.asSyntax.Trigger;
import jason.asSyntax.parser.ParseException;
import jason.asSyntax.Literal;
import jason.asSyntax.LogicalFormula;
import jason.asSyntax.InternalActionLiteral;
import java.util.List;

// Modified from internal action relevant_plans

@SuppressWarnings("serial")
public class relevant_plan_bodies_as_terms extends DefaultInternalAction {

    @Override public int getMinArgs() {
        return 2;
    }
    @Override public int getMaxArgs() {
        return 3;
    }

    @Override
    public Object execute(TransitionSystem ts, Unifier un, Term[] args) throws Exception {
        checkArguments(args);

        Trigger te = null;
        try {
            te = Trigger.tryToGetTrigger(args[0]);
        } catch (ParseException e) {}
        if (te == null)
            throw JasonException.createWrongArgument(this,"first argument '"+args[0]+"' must follow the syntax of a trigger.");

        ListTerm labels = new ListTermImpl();
        ListTerm lt = new ListTermImpl();
        ListTerm last = lt;
        List<Option> rp = ts.relevantPlans(te);
        if (rp != null) {
            for (Option opt: rp) {
                // remove sources (this IA is used for communication)
                Plan np = (Plan)opt.getPlan().clone();
                if (np.getLabel() != null)
                    np.getLabel().delSources();
                np.setAsPlanTerm(true);
                np.makeVarsAnnon();
                
                // This part differs from the relevant_plans internal action
                
                LogicalFormula contextCond = np.getContext();
                if (contextCond == null) {
                	contextCond =  Literal.LTrue;
                }
                
                ListTerm bodiesAsTerms = new ListTermImpl();
                PlanBody pb = np.getBody();
                Pred label = np.getLabel();
                
                if (!pb.isEmptyBody()) {
                	do {
                		Term bodyTerm = pb.getBodyTerm();
                		Term prefix = ASSyntax.createString(pb.getBodyType().toString());
                		             		
                		// HACK ALERT - see first conjunct below
                		if (!label.getFunctor().equals("suspend_purpose") && bodyTerm.isInternalAction()) {
                			// Replace internal action literal
                			Structure bodyIALit = (InternalActionLiteral) bodyTerm;
                			bodyTerm = ASSyntax.createStructure(bodyIALit.getFunctor(), bodyIALit.getTermsArray());
                			prefix = ASSyntax.createString(".");
                		}
                		Literal WrappedBodyTerm = ASSyntax.createLiteral("body_term", prefix, bodyTerm);
                		bodiesAsTerms.add(WrappedBodyTerm);
                		pb = pb.getBodyNext();
                	} while (pb != null);
                }
                Literal plan_lit = ASSyntax.createLiteral("plan", np.getLabel().clone(), np.getTrigger().clone(), contextCond, bodiesAsTerms);
                
                last = last.append(plan_lit);
                
                // End of differences from relevant_plans internal action
                
                if (args.length == 3)
                    labels.add(np.getLabel());
            }
        }

        boolean ok = un.unifies(lt, args[1]); // args[1] is a var;
        if (ok && args.length == 3)
            ok = un.unifies(labels, args[2]);

        return ok;
    }
}
