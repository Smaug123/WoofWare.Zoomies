# Bonsai File Migration Order

This file documents the topological ordering for migrating Bonsai files from OCaml to F# based on their internal dependencies.

## Level 0 (No internal dependencies - migrate first)
- **action.ml/mli** 🔄 TODO → Action.fs
- **annotate_incr.ml/mli** 🔄 TODO → AnnotateIncr.fs
- **apply_action_context.ml/mli** 🔄 TODO → ApplyActionContext.fs
- **keyed.ml/mli** 🔄 TODO → Keyed.fs
- **may_contain.ml/mli** 🔄 TODO → MayContain.fs
- **trampoline.ml/mli** 🔄 TODO → Trampoline.fs

## Level 1

## Level 2
- **environment.ml/mli** → Environment.fs (depends on: import) 
- **input.ml/mli** → Input.fs (depends on: import)
- **meta.ml/mli** → Meta.fs (depends on: import)
- **node_path.ml/mli** → NodePath.fs (depends on: import)
- **path.ml/mli** → Path.fs (depends on: import, keyed)
HUMAN REVIEWED ^
- **stabilization_tracker.ml/mli** → StabilizationTracker.fs (depends on: import, keyed)

## Level 3
- **value.ml/mli** → Value.fs (depends on: environment)
- **lifecycle.ml/mli** → Lifecycle.fs (depends on: path)

## Level 4
- **computation.ml** → Computation.fs (depends on: may_contain, meta, snapshot, trampoline, value)
- **map0.ml/mli** → Map0.fs (depends on: map0_intf, value)
- **snapshot.ml/mli** → Snapshot.fs (depends on: input, lifecycle)
- **var.ml/mli** → Var.fs (depends on: stabilization_tracker, value)

## Level 5
- **eval_sub.ml/mli** → EvalSub.fs (depends on: computation)
- **proc_min.ml/mli** → ProcMin.fs (depends on: computation)
- **simplify.ml/mli** → Simplify.fs (depends on: computation)
- **skeleton.ml/mli** → Skeleton.fs (depends on: computation, node_path)
- **transform.ml/mli** → Transform.fs (depends on: computation, node_path)

## Level 6
- **eval.ml/mli** → Eval.fs (depends on: eval_sub)
- **fix_transform.ml/mli** → FixTransform.fs (depends on: fix_transform_intf)
- **graph_info.ml/mli** → GraphInfo.fs (depends on: transform)
- **incr0.ml/mli** → Incr0.fs (depends on: proc_min)

## Level 7
- **flatten_values.ml/mli** → FlattenValues.fs (depends on: fix_transform)
- **instrumentation.ml/mli** → Instrumentation.fs (depends on: graph_info)
- **proc.ml** → Proc.fs (depends on: incr0, map0, var)

## Level 8
- **constant_fold.ml/mli** → ConstantFold.fs (depends on: fix_transform, proc, simplify)
- **cont.ml/mli** → Cont.fs (depends on: instrumentation, to_dot)
- **linter.ml/mli** → Linter.fs (depends on: skeleton)

## Level 9
- **pre_process.ml/mli** → PreProcess.fs (depends on: constant_fold, flatten_values)
- **proc_layer2.ml/mli** → ProcLayer2.fs (depends on: cont, proc_intf)

## Level 10
- **legacy_api.ml/mli** → LegacyApi.fs (depends on: legacy_api_intf, proc_layer2)
- **to_dot.ml/mli** → ToDot.fs (depends on: pre_process, skeleton)

## Level 11
- **bonsai.ml/mli** → Bonsai.fs (depends on: eval, legacy_api, linter)

## Special Files (migrate separately)
- **driver/bonsai_driver.ml/mli** → Driver/BonsaiDriver.fs (part of driver subpackage)
- **protocol/bonsai_protocol.ml/mli** → Protocol/BonsaiProtocol.fs (part of protocol subpackage)
- **protocol/introspection/bonsai_introspection_protocol.ml/mli** → Protocol/Introspection/BonsaiIntrospectionProtocol.fs

## Key Migration Notes
1. Files with **import** in the name should be IGNORED per CLAUDE.md
2. Each .ml/.mli pair becomes a single .fs file following F# naming conventions
3. Module names are converted from snake_case to PascalCase
4. **import.ml** is foundational but should be skipped - dependencies will need adjustment
5. **computation.ml** is a central abstraction used by many processing files
6. **value.ml** and **environment.ml** form the core value system

## Progress
- ✅ Level 0: action.ml → Action.fs, annotate_incr.ml → AnnotateIncr.fs
- 🔄 Currently working on remaining Level 0 files