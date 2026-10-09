# Benchmarks

Run from the repository root. Use `RE_BENCH_FILTER` to select comma-separated
name prefixes, including a complete phase name:

```sh
RE_BENCH_FILTER='automata/tiny/execp/warm' \
  dune exec --release benchmarks/benchmark.exe -- -quota 55x +time alloc
RE_BENCH_FILTER='edge/nested-repetition/capture-stars/compile' \
  dune exec --release benchmarks/benchmark.exe -- -quota 55x +time alloc
RE_BENCH_FILTER='expression IDs/broad/16/1024/exec/build+compile+exec' \
  dune exec --release benchmarks/benchmark.exe -- -quota 55x +time alloc
RE_BENCH_FILTER='gitignore/latex-ignore-suffixes/noncapturing' \
  dune exec --release benchmarks/stats.exe
```

**Filter large cases deliberately.** Some expression-ID workloads require several
GiB. Filtering uses metadata only; unselected workloads are not constructed.
`stats.exe` selects workload names, not phase names.

## What one iteration measures

Each input workload supplies `exec`, `execp`, and `exec_opt` modes. `exec` catches
`Not_found` for negative samples. Custom operations such as splitting have a
`run` mode. Correctness checks happen outside the timed functions.

| Phase | Timed work |
| --- | --- |
| `compile` | Compile an already constructed `Re.t`. |
| `MODE/compile+exec` | Compile that `Re.t` and run the workload on a fresh regex. |
| `MODE/cold` | `copy_re` an unexecuted template and run the workload. Includes the copy. |
| `MODE/warm` | Run a regex previously warmed with this same workload. |
| `parse` | Construct a `Re.t` with the Perl parser (and `no_group`, where requested). |
| `MODE/parse+compile+exec` | Parse, compile, and run. |
| `build+compile` | Construct a generated AST and compile it, without matching. |
| `MODE/build+compile+exec` | Construct that AST, compile, and run. |

Only Perl workloads have `parse` phases; generated expression-ID workloads have
`build+compile` phases. Their input strings are prepared once, outside either phase.

An input workload runs **all its samples** in one iteration, sharing the regex
within the iteration. `uri` is a batch; `uri/input/1`, `/2`, and `/3` retain the
original isolated-input measurements. The other original small matching cases
have one sample each. New matching, common-pattern and edge-case families use batches.

### Comparing with the old drivers

This is a consolidation of measurement methodology, not just a rename:

- The former expression-ID `comp` and `comp+exec` phases included AST
  construction. Their counterparts are `build+compile` and
  `MODE/build+compile+exec`; `compile` alone is deliberately narrower.
- The old general `exec` phase copied mutable regex state. That measurement is
  now explicitly called `cold`; `warm` is a different measurement.
- The old small-case runner benchmarked each input separately. Use the isolated
  URI cases rather than the batch when comparing those results.
- The legacy HTTP regex is deliberately preserved, including its liberal
  header key that can consume newlines: it matches the entire pinned corpus
  as one match even in the manual/single-request workload.

Do not compare timings solely by similarly named phases. The common runner
also adds modes to workloads that previously exposed only one matching API.

## Registration, checks, and size snapshots

`Suite.cases` is the registry for both runners and tests. Workloads use these
constructors from `Workload`:

- `inputs`: a lazy pattern and `(input, expected_match)` pairs;
- `perl`: a lazy source string and the same sample pairs;
- `generated`: separate lazy AST and sample builders, so rebuilding does not
  allocate the input strings;
- `custom`: a lazy pattern, timed operation, and required correctness check.

These constructors supply their own mode and construction-phase metadata.
`yes` and `no` build sample pairs. Additional capture checks can be attached to
input workloads. The nested-history matrix reuses `Capture_histories.nested`.

Before timing, the runner compiles a separate regex, reports its size after
forcing, and checks it. This never warms the timed regex. `forcing: full` means
all states were explored; `forcing: inputs` means only the supplied workloads
were executed because full exploration is impractical. Reported words count
the reachable compiled regex graph, not the input corpus or total allocation.

`dune runtest benchmarks` checks all registered workloads except those explicitly
marked `runtest:false` (currently the eight-million-ID case). Checks cover cold,
warm, copied, and freshly reconstructed patterns; reconstruction must also
produce the advertised AST. Exact forced-size snapshots are pinned to 64-bit
OCaml 5.4.1. The smoke rules exercise the real runner and phase filtering.
