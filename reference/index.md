# Package index

## Steplists

- [`launch_steplist_creator()`](https://forsterepi.github.io/epicmodel/reference/launch_steplist_creator.md)
  :

  Launch steplist creator `shiny` app

- [`new_steplist()`](https://forsterepi.github.io/epicmodel/reference/epicmodel_steplist.md)
  [`validate_steplist()`](https://forsterepi.github.io/epicmodel/reference/epicmodel_steplist.md)
  [`empty_steplist()`](https://forsterepi.github.io/epicmodel/reference/epicmodel_steplist.md)
  [`print(`*`<epicmodel_steplist>`*`)`](https://forsterepi.github.io/epicmodel/reference/epicmodel_steplist.md)
  [`print(`*`<epicmodel_steplist_checked>`*`)`](https://forsterepi.github.io/epicmodel/reference/epicmodel_steplist.md)
  [`summary(`*`<epicmodel_steplist>`*`)`](https://forsterepi.github.io/epicmodel/reference/epicmodel_steplist.md)
  [`summary(`*`<epicmodel_steplist_checked>`*`)`](https://forsterepi.github.io/epicmodel/reference/epicmodel_steplist.md)
  [`plot(`*`<epicmodel_steplist>`*`)`](https://forsterepi.github.io/epicmodel/reference/epicmodel_steplist.md)
  [`plot(`*`<epicmodel_steplist_checked>`*`)`](https://forsterepi.github.io/epicmodel/reference/epicmodel_steplist.md)
  : Steplist objects

- [`check_steplist()`](https://forsterepi.github.io/epicmodel/reference/check_steplist.md)
  :

  Check `epicmodel_steplist` class objects

## Processing steplists

- [`remove_all_modules()`](https://forsterepi.github.io/epicmodel/reference/remove_all_modules.md)
  : Remove all modules

- [`remove_na()`](https://forsterepi.github.io/epicmodel/reference/remove_na.md)
  :

  Removing NA in `icc` and `outc`

- [`remove_segment()`](https://forsterepi.github.io/epicmodel/reference/remove_segment.md)
  : Remove segments

- [`uncheck_steplist()`](https://forsterepi.github.io/epicmodel/reference/uncheck_steplist.md)
  :

  Unchecking `epicmodel_steplist` objects

## SCC models

- [`create_scc()`](https://forsterepi.github.io/epicmodel/reference/create_scc.md)
  : Creating SCC models
- [`new_scc()`](https://forsterepi.github.io/epicmodel/reference/epicmodel_scc.md)
  [`validate_scc()`](https://forsterepi.github.io/epicmodel/reference/epicmodel_scc.md)
  [`empty_scc()`](https://forsterepi.github.io/epicmodel/reference/epicmodel_scc.md)
  [`print(`*`<epicmodel_scc>`*`)`](https://forsterepi.github.io/epicmodel/reference/epicmodel_scc.md)
  [`summary(`*`<epicmodel_scc>`*`)`](https://forsterepi.github.io/epicmodel/reference/epicmodel_scc.md)
  [`plot(`*`<epicmodel_scc>`*`)`](https://forsterepi.github.io/epicmodel/reference/epicmodel_scc.md)
  : SCC model objects

## Inspecting SCC models

- [`are_sufficient()`](https://forsterepi.github.io/epicmodel/reference/are_sufficient.md)
  : Check if a certain set of component causes is suffcient
- [`scc_cause_sets()`](https://forsterepi.github.io/epicmodel/reference/scc_cause_sets.md)
  : Extracting component causes from SCC model
- [`sc_contain_steps()`](https://forsterepi.github.io/epicmodel/reference/sc_contain_steps.md)
  : Do steps appear in sufficient causes?
- [`show_steps()`](https://forsterepi.github.io/epicmodel/reference/show_steps.md)
  : Show all steps of a SCC model
- [`necessary_causes()`](https://forsterepi.github.io/epicmodel/reference/necessary_causes.md)
  : Extract necessary causes

## Using SCC models

### Standardized effect size

- [`effect_size()`](https://forsterepi.github.io/epicmodel/reference/effect_size.md)
  : Determine standardized effect size of component causes

### Mechanisms

- [`mechanism()`](https://forsterepi.github.io/epicmodel/reference/mechanism.md)
  [`new_mechanism()`](https://forsterepi.github.io/epicmodel/reference/mechanism.md)
  [`validate_mechanism()`](https://forsterepi.github.io/epicmodel/reference/mechanism.md)
  [`plot(`*`<epicmodel_mechanism>`*`)`](https://forsterepi.github.io/epicmodel/reference/mechanism.md)
  [`print(`*`<epicmodel_mechanism>`*`)`](https://forsterepi.github.io/epicmodel/reference/mechanism.md)
  : Investigate mechanisms
- [`export_mechanism()`](https://forsterepi.github.io/epicmodel/reference/export_mechanism.md)
  : Export mechanisms

### Intervention and Prevention

- [`intervene()`](https://forsterepi.github.io/epicmodel/reference/intervene.md)
  : Explore effect of interventions
- [`prevent()`](https://forsterepi.github.io/epicmodel/reference/prevent.md)
  : Explore effect of prevention

### DAGs

- [`scc_to_dag()`](https://forsterepi.github.io/epicmodel/reference/scc_to_dag.md)
  : Transform SCC to DAG
- [`plot_dag()`](https://forsterepi.github.io/epicmodel/reference/plot_dag.md)
  : Plot DAG

## Built-in data

- [`steplist_party`](https://forsterepi.github.io/epicmodel/reference/steplist_party.md)
  : Birthday party example steplist
- [`steplist_rain`](https://forsterepi.github.io/epicmodel/reference/steplist_rain.md)
  : Rain example steplist
- [`scc_rain`](https://forsterepi.github.io/epicmodel/reference/scc_rain.md)
  : Rain example SCC model
