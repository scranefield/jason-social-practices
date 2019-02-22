/* Initial beliefs and rules */

// TO DO: implement support for optional vs. compulsory landmarks.

relevant_sp(SP) :- social_practice(SP, Requirements) & forall(Requirements).

// Stick with currently selected option, if there is one
sp_selection(Options, CurrentSP) :- selected_sp(CurrentSP) & .member(CurrentSP, Options).
// Simple default policy - choose first option
sp_selection([SP|_], SP).


forall([]).
forall([LogicalExp|T]) :- LogicalExp & forall(T).

recursion_depth_bound(3).

has_plan_generating_action(Trigger, Action, BodyTerms, Path) :-
	//.print("Calling ", has_plan_generating_action(Trigger, Action, BodyTerms, Path)) &
    .findall(tuple(Depth, BodyTerms, Path),
             (has_plan_generating_action(Trigger, Action, true, Trigger, 0, [], BodyTerms, Depth, Path)
             //	& .print("Found a plan; Path = ", Path)
             ),
             Results) &
    .sort(Results, Sorted) &
    //.print("Sorted results: ", Sorted) &
    Sorted = [tuple(_, BodyTerms, Path)|_].

has_plan_generating_action(Trigger, Action, CheckContext, OrigTrigger, CurrDepth, CurrPath, BodyTerms, Depth, Path) :-
	meta.relevant_plan_bodies_as_terms(Trigger, RPlans) &
	//.print("Relevant plans: ", RPlans) &
	some_plan_generates_action(RPlans, Action, CheckContext, OrigTrigger, CurrDepth, CurrPath, BodyTerms, Depth, Path)
	// & .print("Found path ", Path)
	.
	
some_plan_generates_action(Plans, Action, CheckContext, OrigTrigger, CurrDepth, CurrPath, BodyTerms, Depth, Path) :-
    .nth(N, Plans, plan(_,_,Context,BodyTerms)) &
    //not Label =.. [suspend_purpose, _, _] &
    //.print("Checking plan with Context ", Context, " and body terms ", BodyTerms) &
    ((not CheckContext) | (CheckContext & Context)) &
    .concat(CurrPath, [N], NewCurrPath) & // TO DO: use difference list or reversed list instead
    plan_body_generates_action(BodyTerms, Action, OrigTrigger, CurrDepth, NewCurrPath, Depth, Path)
    // & .print("Plan with body terms ", BodyTerms, " checked out against ", Action)
    .

plan_body_generates_action(BodyTerms, Action, OrigTrigger, CurrDepth, CurrPath, Depth, Path) :-
    .member(BT, BodyTerms) &
    //.print("Checking body term ", BT, " against ", Action) &
    plan_body_term_generates_action(BT, Action, OrigTrigger, CurrDepth, CurrPath, Depth, Path)
    // & .print("Body term ", BT, " checked out against ", Action, ": Path = ", Path)
    .

plan_body_term_generates_action(body_term(".", Action), Action, _, Depth, Path, Depth, Path).
plan_body_term_generates_action(body_term("", Action), Action, _, Depth, Path, Depth, Path).
plan_body_term_generates_action(body_term("!", solve([body_term(".", Action)])), Action, _, Depth, Path, Depth, Path).
plan_body_term_generates_action(body_term("!", solve([body_term("", Action)])), Action, _, Depth, Path, Depth, Path).
plan_body_term_generates_action(body_term("!", Goal), Action, OrigTrigger, CurrDepth, CurrPath, Depth, Path) :-
	//.print("Recursive case; CurrPath = ", CurrPath) &
	not Goal = solve(_) &
    not OrigTrigger = {+!Goal} & // Prevent self-recursion
    recursion_depth_bound(RD) &
    CurrDepth < RD &
    has_plan_generating_action({+!Goal}, Action, false, OrigTrigger, CurrDepth+1, CurrPath, _, Depth, Path) &
    .print("Recursion returned; Path = ", Path).    

/* Initial goals */

!metaDeliberate.

/* Plans */

@metaplan[atomic]
+!metaDeliberate <-
	.print("metaDeliberate plan called");	
	.findall(Key=Value, state(Key,Value), StateList);
	.print("State: ", StateList);
	.findall(ResName, resource(ResName), ResNames);
	.print("Resources: ", ResNames);
	.findall(SP, ( relevant_sp(SP) & not completed_sp(SP) ), RelevantSPs);
	.print("Relevant SPs: ", RelevantSPs);
	if (RelevantSPs == []) {
		if (selected_sp(CurrentlySelectedSP)) {
			-selected_sp(CurrentlySelectedSP);	
		}
	} else {
		if (sp_selection(RelevantSPs, SelectedSP) & not selected_sp(SelectedSP)) {
			.print("Newly selected SP: ", SelectedSP);
			-+selected_sp(SelectedSP);
		}
		for (monitored(Purpose, SP, ID)) {
			.print("Checking monitored purpose ", monitored(Purpose, SP, ID), " (potential timeout is not checked)");
			if (_[assumed] = Purpose) {
				// TO DO: THIS NEEDS SOME THOUGHT - what if action hasn't completed yet?
				.print("Purpose ", Purpose, " is assumed to have been achieved");
				.succeed_goal(Purpose);
				+completed_landmark(SP, ID, Purpose);
			} elif (Purpose) {
				.print("Purpose ", Purpose, " has been achieved");
				+completed_landmark(SP, ID, Purpose);				
			} 
		}
	}
	.wait(200);
	!!metaDeliberate.
	
//^!G[state(finished)] <- .print("Goal ", G, " has succeeeded").

+selected_sp(SP) <-
	.print("New SP selection: ", SP);
	for (landmark(SP, ID, _, _, Purpose)) {
		PurposeNoAnnots[dummy] = Purpose[dummy];
		.print("Checking if purpose ", PurposeNoAnnots, " for landmark ", ID, " is already intended");
		if (.intend(PurposeNoAnnots)) {
			.print("Purpose ", PurposeNoAnnots, " is already intended - suspending it");
			.suspend(PurposeNoAnnots);
			+suspended_intention(SP, ID, PurposeNoAnnots);
		}
		.add_plan({ @suspend_purpose(SP,ID) +!PurposeNoAnnots <- .print("Suspending goal ", PurposeNoAnnots); .suspend(PurposeNoAnnots) }, landmark(SP,ID), begin);
	}
	for (landmark(SP, ID, [], Actions, Purpose)) {
		!activate_landmark(SP, ID, Actions, Purpose);
	}.
	
@activate_landmark[atomic]
+!activate_landmark(SP, ID, Actions, Purpose) : .my_name(Me) <- 
	.print("Active landmark: ", landmark(SP, ID, _, Actions, Purpose));
	PurposeNoAnnots[dummy] = Purpose[dummy];
	if (not _[assumed] = Purpose) { // Note: we don't yet have assumed landmark purposes in our scenario
		.print("Monitoring purpose ", PurposeNoAnnots);
		+monitored(PurposeNoAnnots, SP, ID);
	}
	if (Actions = [action(Actors, Act)] & 
		(Actors = Me | (.list(Actors) & .member(Me, Actors)))) { // Currently handle single action only	
		.print("Calling ", Act, " due to landmark ", ID);
		if (has_plan_generating_action({+!Purpose}, Act, BodyTerms, Path)) {
			.print("Found plan to call to achieve purpose ", Purpose, " ; Path=", Path);
			!!solve([body_term("!",Purpose)], Path);	
		} elif (joint(Act)) {
			!!solve([body_term("", Act[participants(Actors)])])
		} elif (durative(Act)) {
			!!solve([body_term("", Act)])
		} elif (Act =.. [F, _, _] & .substring(".", F)) {
			!!solve([body_term(".", Act)])
		} else { Act }
	} else {
		.print("Only singleton lists of actions are currently supported in landmarks");
	}.
	
-selected_sp(SP) <- .print("Active SP ", SP, " is no longer selected - NOT HANDLED").
// TO DO: check if any landmarks are active and deactivate them.

@completed_landmark[atomic]
+completed_landmark(SP, ID, Purpose) <-
  	.print("Deactivating completed landmark ", landmark(SP, ID, Purpose));
  	.succeed_goal(Purpose);
  	-monitored(Purpose, SP, ID);
  	.remove_plan(suspend_purpose(SP,ID));
  	for ( landmark(SP, ID2, PrecedingLMs, Actions, Purpose2) &
		  .print("Checking whether landmark ", landmark(SP, ID2, PrecedingLMs, Actions, Purpose2), " is next.") &
		  not completed_landmark(SP, ID2, _) &
		  .findall(PrecID, ( .member(PrecID, PrecedingLMs) & completed_landmark(SP, PrecID, _) ), CompletedPrecIDs) &
		  .difference(PrecedingLMs, CompletedPrecIDs, []) ) {
		.print("About to activate landmark ", ID2);
		!activate_landmark(SP, ID2, Actions, Purpose2);
	}
	.findall(ID2, ( landmark(SP, ID2, _, _,_) & not completed_landmark(SP, ID2, _) ), PendingLandmarks);
	if (PendingLandmarks == []) {
		+completed_sp(SP);
		.print("SP ", SP, " has finished - any housekeeping to do?");
	}.

{ include("metainterpreter.asl") }
