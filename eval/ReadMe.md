# Evaluation

Reproduces the evaluation of the paper. `paper.py` writes its figures. The expected results of every
experiment, with the variation to expect, are in [RESULTS.md](RESULTS.md).

```
dune build                      # in ..
tools/setup.sh                  # builds Ruler, Twee and the Python environment in tools
./run.sh all                    # or individual experiments, see below
tools/venv/bin/python report.py # figures in out/figures, tables in out/results/report.md
```

| script | purpose |
|---|---|
| `run.sh` | one function per experiment; tool paths and the time budget via environment variables |
| `terms.py` | random terms, uniform over shapes, reproducible by seed |
| `convert.py` | rules between our format, Ruler's JSON and Twee's output; `cap N` extracts the size-N system |
| `egraph.py` | equality saturation with egglog (one e-graph per term) |
| `stats.py` | mean, median, maximum, distribution of simplified terms  |
| `ground_check.py` | ground completeness of bool systems on all ground terms up to a size |
| `report.py` | figures and tables |
| `paper.py` | writes the results into the coordinates of the paper's figures (`LATEX_DIR/eval/*.tex`) |

`./run.sh all` takes about two hours. Experiments build on earlier ones
(`all` lists them in a valid order) and cache their outputs in `out`. Run them
one at a time: Ruler with six iterations needs about 20 GB, and the budget runs
are capped at 10 GB each (`MEMORY_KB`).

`run.sh` calls the binary with `--lower-vars`, the file format the scripts use
(variables lowercase, constants uppercase). The paper writes it the other way round.

External tools are built by `tools/setup.sh` into `tools/build`, `tools/bin` and
`tools/venv` (needs git, cargo, cmake, g++, cabal with GHC 9.12.2 in `GHC`, Python
3.14); `run.sh` takes their paths from `RULER`, `EGGLOG_PY`, `TWEE` and `TWEE_AXIOMS`:

| tool | source |
|---|---|
| Ruler (OOPSLA'21) | https://github.com/uwplse/ruler at commit `e4217ed4`, with our int domain `tools/ruler/int.rs` and `tools/ruler/Cargo.lock` |
| Twee 2.6 | https://github.com/nick8325/twee at commit `9dc5b610`, with the dependency versions of `tools/twee/cabal.project.freeze` |
| egglog, matplotlib, z3 | PyPI, pinned by hash in `tools/requirements.txt` |
| Twee's axioms of the boolean theory | `bool_complete_generator.p` |

| experiment | paper | content |
|---|---|---|
| `synthesis` | Fig. 1, Table 2 | bool with 3 constants, without and with 3 variables, to saturation |
| `budget` | Section 6.6 | int, bv4, bv32 within 10 minutes per system |
| `coverage` | Table 1 | Ruler iterations 2–6 and mutual derivability with our size-5/7/9 systems |
| `greedy500` | Fig. 3 | greedy simplification of 1000 terms of size 500 |
| `ruler50` | Fig. 5 | greedy and e-graph simplification of 1000 terms of size 50, Ruler vs ours |
| `large` | Fig. 4 | terms of size 50 to 3000: greedy vs e-graph |
| `twee` | Fig. 6 | Twee completion vs ours |
| `sweep` | Figs. 7, 8 | simplification by maximal rule size for bool, int, bv4, bv32, with Ruler in an e-graph |
| `noise` | – | run-to-run variation of egglog's results on large terms |
| `constants` | Section 6.7 (threats to validity) | systems for 2–4 constants applied to terms with 2–10 constants |
| `validate` | – | test suite and ground completeness of the saturated systems |

## Docker

`docker/Dockerfile` builds all tools with pinned versions and runs `run.sh`:

```
docker build -f eval/docker/Dockerfile -t rule-enum-eval .      # in .., about 10 minutes
mkdir -p eval/out-docker                                        # else Docker creates it owned by root
docker run --rm -u "$(id -u):$(id -g)" -v "$PWD/eval/out-docker:/eval/out" rule-enum-eval      # all, or experiments
docker run --rm -u "$(id -u):$(id -g)" -v "$PWD/eval/out-docker:/eval/out" --entrypoint python3 rule-enum-eval report.py
```

| component | pin |
|---|---|
| base images | `rust:1.88.0`, `haskell:9.12.2`, `ocaml/opam:debian-12-ocaml-5.2`, `python:3.14.7` (Debian 12), by digest |
| Debian packages | `snapshot.debian.org` at `DEBIAN_SNAPSHOT` (`docker/apt-snapshot.sh`) |
| Ruler | commit `e4217ed4` with `tools/ruler` |
| Twee | commit `9dc5b610` with `tools/twee/cabal.project.freeze` |
| our implementation | OCaml 5.2.1, dune 3.20.2 (the test suite runs during the build) |
| Python | egglog 13.2.0, matplotlib 3.11.1, z3-solver 4.16.0.0 (provides `z3`), by hash in `tools/requirements.txt` |

Ruler is compiled for generic x86-64, not with the repository's `target-cpu=native`.
Give Docker enough memory for Ruler with six iterations (about 20 GB, `coverage`).

The e-graph on large terms and twee at size 9 vary from run-to-run.
The bv4 system at size 8 completes within 589-595s out of the allotted 600s.
