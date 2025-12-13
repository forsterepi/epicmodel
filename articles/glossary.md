# Glossary

- **Condition**: The term “condition” is used for IF and IFNOT parts of
  steps (*IF conditions*, *IFNOT conditions*). See
  [`vignette("steplist")`](https://forsterepi.github.io/epicmodel/articles/steplist.md).
- **CC**: Component cause. A *step type*. Component causes are *start
  steps*, i.e., they have no *IF condition* but they might have an
  *IFNOT condition*. Unlike *interventions*, they appear in *IF
  conditions* of other *steps*. See
  [`vignette("steplist")`](https://forsterepi.github.io/epicmodel/articles/steplist.md).
- **DAG**: Directed acyclic graph. In maths, graphs are networks
  consisting of nodes and edges. `Directed` means that edges are arrows.
  `Acyclic` means that, when you follow the arrows, you never get back
  to the same node. DAG is the form used for causal graphs. (Nodes are
  variables. Edges are causal relations. Causal relations are asymmetric
  =\> directed. Variables cannot cause themselves =\> acyclic.)
- **DOES**: The second *segment* in the `WHAT DOES WHAT WHERE` structure
  of *THEN statements*. Refers usually to a verb in a sentence. See
  [`vignette("steplist")`](https://forsterepi.github.io/epicmodel/articles/steplist.md).
- **End step**: A *step type*. *Outcome definitions* consist of end
  steps combined with AND/OR logic.
- **epicmodel**: Causal Modeling in Epidemiology
- **IF condition**: The condition that has to be fulfilled for a *step*
  to occur. See
  [`vignette("steplist")`](https://forsterepi.github.io/epicmodel/articles/steplist.md).
- **IFNOT condition**: The condition that must **not** be fulfilled for
  a step to occur. See
  [`vignette("steplist")`](https://forsterepi.github.io/epicmodel/articles/steplist.md).
- **ICC**: Incompatible component causes. If a pair of *component
  causes* cannot be part of the same *sufficient cause*, they can be
  defined as ICC. Sets of *component causes*, which include both, are
  assumed to be invalid and are not checked during *SCC* model creation.
  See
  [`vignette("steplist")`](https://forsterepi.github.io/epicmodel/articles/steplist.md).
- **Intervention**: A *step type*. Interventions are *start steps*,
  i.e., they have no *IF condition* but they might have an *IFNOT
  condition*. Unlike *component causes*, they do **not** appear in *IF
  conditions* of other *steps* but only in *IFNOT conditions*. See
  [`vignette("steplist")`](https://forsterepi.github.io/epicmodel/articles/steplist.md).
- **Module**: *Steps* can be grouped together into modules, which might
  be informative for certain *SCC* models. See
  [`vignette("modules")`](https://forsterepi.github.io/epicmodel/articles/modules.md).
- **Minimal**: A *sufficient cause (SC)* is minimal, if there exists no
  smaller SC. SC1 is smaller than SC2 if SC1 only contains *component
  causes* that are also in SC2, but not all of them. See
  [`vignette("scc")`](https://forsterepi.github.io/epicmodel/articles/scc.md).
- **Object**: The third *segment* in the `WHAT DOES WHAT WHERE`
  structure of *THEN statements*. It is usually a *WHAT* *segment*, but
  also may be a *THEN statement* if a *DOES* *segment* with
  `then_object_does == 1` is used. See
  [`vignette("steplist")`](https://forsterepi.github.io/epicmodel/articles/steplist.md).
- **Outcome definition**: It defines based on the occurrence of certain
  *steps* that the outcome of interest has occurred. See
  [`?create_scc`](https://forsterepi.github.io/epicmodel/reference/create_scc.md).
- **Prevention**: The avoidance of (a set of) *component causes*.
- **SC**: Sufficient cause. A *minimal* set of *component causes*, which
  fulfills the *outcome definition*. See
  [`vignette("scc")`](https://forsterepi.github.io/epicmodel/articles/scc.md).
- **SCC**: Sufficient-component cause. The causal modeling framework at
  the core of `epicmodel`. For an explanation of what “this thing called
  SCC model” is to `epicmodel`, see
  [`vignette("scc")`](https://forsterepi.github.io/epicmodel/articles/scc.md).
- **Scenario**: When creating *IF conditions* and *IFNOT conditions*,
  *steps* within the same scenario are combined with AND, while the
  scenarios themselves are combined with OR.
- **Segment**: *THEN statements* follow a `WHAT DOES WHAT WHERE`
  structure. These elements, i.e., *WHAT*, *DOES*, and *WHERE*, are
  called segments. See
  [`vignette("steplist")`](https://forsterepi.github.io/epicmodel/articles/steplist.md).
- **Start step**: A *step type*. They have no *IF condition* but they
  might have an *IFNOT condition*. *Component causes* and
  *interventions* are start steps. See
  [`vignette("steplist")`](https://forsterepi.github.io/epicmodel/articles/steplist.md).
- **Statement**: The term “statement” is used for THEN parts of steps
  (*THEN statement*). See
  [`vignette("steplist")`](https://forsterepi.github.io/epicmodel/articles/steplist.md).
- **Step**: Steps are small parts of the complex mechanism that leads to
  outcome occurrence. See
  [`vignette("steplist")`](https://forsterepi.github.io/epicmodel/articles/steplist.md).
- **Step types**: *Steps* can play different roles in *SCC* model
  creation. Examples are *start steps*, *component causes*,
  *interventions*, *end steps*, IFNOT steps, as well as non-starting
  steps. IFNOT steps are *steps* with *IFNOT condition*. Non-starting
  steps are *steps* with *IF condition*. See
  [`vignette("steplist")`](https://forsterepi.github.io/epicmodel/articles/steplist.md).
- **Steplist**: The steplist is the collection of all *steps* in a
  pre-defined format. It is the only input for *SCC* model creation. See
  [`vignette("steplist")`](https://forsterepi.github.io/epicmodel/articles/steplist.md).
- **Subject**: The first *WHAT* *segment* in the `WHAT DOES WHAT WHERE`
  structure of *THEN statements*. See
  [`vignette("steplist")`](https://forsterepi.github.io/epicmodel/articles/steplist.md).
- **THEN**: THEN statements are a part of *steps* and describe what
  actually happens. The THEN statement happens if the *IF condition* is
  fulfilled and if the *IFNOT condition* is not fulfilled. See
  [`vignette("steplist")`](https://forsterepi.github.io/epicmodel/articles/steplist.md).
- **WHAT**: *Segments* of the *THEN statements*, which are used in
  *subjects* and *objects* and represent people, things, concepts, etc.
  See
  [`vignette("steplist")`](https://forsterepi.github.io/epicmodel/articles/steplist.md).
- **WHERE**: The last *segment* in the `WHAT DOES WHAT WHERE` structure
  of *THEN statements*. Refers usually to a location. See
  [`vignette("steplist")`](https://forsterepi.github.io/epicmodel/articles/steplist.md).
