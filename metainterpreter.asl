	// Metainterpreter plans in project careRobotWithSPs
	// To be included in agent asl files
	
	// TO DO: Consider whether any metainterpreter plans need an all_unifs annotation
	
	context_ok(plan(_,_,ContextCond,_)) :- ContextCond.
	
	filter_list([], _, []).
	filter_list([H|T], KeepElementPred, [H|T2]) :-
		Pred =.. [KeepElementPred, [H], []] &
	    Pred &
	    filter_list(T, KeepElementPred, T2).
	filter_list([H|T], KeepElementPred, T2) :-
	    Pred =.. [KeepElementPred, [H], []] &
	    not Pred &
	    filter_list(T, KeepElementPred, T2).
	
	internal_actions([print, wait]).
	
	is_internal_action(T) :-
		T =.. [F, _, _] &
		internal_actions(IAs) &
		.member(F, IAs).
	
	+!solve(PlanBodyTerms) <- !solve(PlanBodyTerms, [], 1, no_path) .
	+!solve(PlanBodyTerms, Path) <- !solve(PlanBodyTerms, [], 1, Path) .
	
	+!solve([], _, _, _).
	+!solve([body_term(Prefix, Term)|BTs], Intn, N, Path) <-
		Intn2 = [N|Intn];
		.print("Calling ", solve(Prefix, Term, Intn2, Path));
		!solve(Prefix, Term, Intn2, Path);
		!solve(BTs, Intn, N+1, Path).
	
	+!solve("?", B, _, _) <- ?B.
	+!solve("+", B, _, _) <- +B.
	+!solve("-", B, _, _) <- -B.
	+!solve("!", solve(PBTs), Intn, Path) <-
		!solve(PBTs, Intn, 1, Path).
	+!solve("!", G, Intn, Path) <-
		meta.relevant_plan_bodies_as_terms({+!G}, RPlans);
		.print("RPlans = ", RPlans);
		if (.list(Path) & Path = [N|PathTail] & .nth(N, RPlans, plan(Label,_,_,PlanBodyTerms))) {
			!solve(PlanBodyTerms, [Label|Intn], 1, PathTail);
		} else {
			?filter_list(RPlans, context_ok, APlans);
			!solve_one(APlans, Intn, Path);
		}.
	
	// Below, the order of arguments to "=" is important (see https://github.com/jason-lang/jason/blob/master/doc/tech/annotations.adoc#unification-between-variables)
	// TO DO: Only pass on the environment the annotations that it needs to see 
	+!solve("", AnnotatedAction, _, _) : Act[dummy] = AnnotatedAction[dummy] & durative(Act) <- // Act[continue(Query)] = AnnotatedAction & durative(Act) & .print("Solving for durative action") <-
		?durative_action_continuation_pred(Act, Query);
		if (durative_action_cleanup_goal(Act, CleanupGoal)) {
			CUGoal = CleanupGoal;
		} else {
			CUGoal = true;
		}
		if (joint(Act) & Act[participants(P)] = AnnotatedAction) {
			ParticipantAnnotation = [participants(P)];
		} else {
			ParticipantAnnotation = [];
		}
		if (time(T)) { Act[durative|ParticipantAnnotation]; -+started(Act, T)[source(meta)]; } // Does use of "if" mean Act is executed in same cycle as time query?
		!solve_durative(Query, Act, ParticipantAnnotation, CUGoal).
	+!solve("", Action, _, _) <- Action.
	+!solve(".", .fail, _, _) <- .fail.
	+!solve(".", .print(S), _, _) <- .print(S).
	+!solve(".", .wait(Cond), _, _) <- .wait(Cond).
	+!solve(".", .wait(Cond, Timeout), _, _) <- .wait(Cond, Timeout).
		
	+!solve_one([plan(Label,_,_,PlanBodyTerms)|_], Intn, Path) : not tried_plan(Label, Intn) <-
	    +tried_plan(Label, Intn);
		!solve(PlanBodyTerms, [Label|Intn], 1, Path);
		-tried_plan(Label, Intn).
		//+plan_succeeded(Label).
			
	-!solve_one([_|PlanTerms], Intn, Path) <-
		!solve_one(PlanTerms, Intn, Path). // Was !!, but why?
	
	+!solve_durative(Query, Act, ParticipantAnnotation, CleanupGoal) <-
	    if (Query) {
			Act[durative|ParticipantAnnotation];
			!solve_durative(Query, Act, ParticipantAnnotation, CleanupGoal);
		} else {
			stop(Act)[durative|ParticipantAnnotation];
			if (CleanupGoal \== true) {
				!CleanupGoal;
			}
		}.
	-!solve_durative(_, Act, _) <-
		-started(Act, _).
	
	
