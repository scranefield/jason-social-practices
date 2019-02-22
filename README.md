# jason-social-practices
A metadeliberation plan and associated [Jason](https://github.com/jason-lang/jason) metainterpreter enabling Jason BDI agents to act in accordance with social practices.

To use in a Jason program, include the following preprocessor directive in your agent source (asl) files:
```
{ include("metadeliberation.asl") }

```
Your agent code should include facts defining the social practices and their landmarks, in the format:
```
social_practice(SPName, ListOfActivationConditions).
landmark(SPName, Landmark1Name, PriorLandmarkList1, [action(ActorList1 Action1)], LM1PurposeLiteral).
landmark(SPName, Landmark2Name, PriorLandmarkList2, [action(ActorList2,Action2)], LM2PurposeLiteral).
...
```

If using the metainterpreter alone, use this preprocessor directive:
```
{ include("metainterpreter.asl") }
```

To come: agent and environment for an example scenario.

For more information, see [arXiv paper link to come].
