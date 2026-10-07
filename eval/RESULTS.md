# Expected results

This document lists, for every experiment of `run.sh`, what it measures, which figure or table of the paper it produces, and the values the paper reports. How to set up and run the experiments is described in [ReadMe.md](ReadMe.md).

Most results are deterministic: rule systems, rule counts, normal forms, coverage counts and all greedy simplification results are reproduced exactly, independent of the machine. Two kinds of results vary from run to run:

- **e-graph results on large terms** (egglog): extraction from large e-graphs
  depends on the run, so medians and means vary by up to about 15 %;
- **Twee at its resource limit** (size 9): the reported system differs slightly.

Times are  on AMD Ryzen 7 3700X, 8 cores, 64 GB and
scale with the machine. The experiments are single-threaded. Ranges are taken over four complete runs.

Rules and terms are quoted in the file format of the evaluation (`--lower-vars`:
variables lowercase, constants uppercase), the reverse of the paper's notation.
`report.py` writes the figures to `out/figures` and all tables to
`out/results/report.md`.

| experiment | paper | runtime | varies between runs |
|---|---|---|---|
| `synthesis` | Fig. 1, Table 2 | 17 min | – |
| `budget` | Section 6.6 | 20 min | see the note on bv4 below |
| `coverage` | Table 1 | 20 min | – |
| `greedy500` | Fig. 3 | 1 min | – |
| `ruler50` | Fig. 5 | 2 min | – |
| `large` | Fig. 4 | 10 min | e-graph values |
| `twee` | Fig. 6 | 1 min | Twee at size 9 |
| `sweep` | Figs. 7, 8 | 17 min | Ruler (e-graph) points |
| `noise` | – | 10 min | (measures the variation) |
| `constants` | Section 6.7 | 1 min | – |
| `validate` | – | 1 min | – |

## `synthesis`: complete systems for the boolean theory (Fig. 1, Table 2)

The boolean theory with three constants, without and with three variables, is
synthesized until no larger left-hand sides exist; the resulting systems are
complete for all ground terms. Output: `out/synth/bool_v0c3.*` (constants) and
`out/synth/bool_vcs3.*` (with variables): per-size statistics (`.csv`), rules,
normal forms (`.irs`) and log.

| | complete at size | normal forms | rules | of which same-size (KBO) | enumerated terms | time |
|---|---|---|---|---|---|---|
| constants only | 21 | 232 | 200953 | 1013 | 201185 | 3.0 s |
| with variables | 23 | 2452 | 1670537 | 554 | 12433926 | 17.3 min (16.5–17.3) |

Rules after each size (Table 2). Times are cumulative synthesis times. 

| size | 5 | 7 | 9 | 10 | 12 | 15 | 18 | 21 | 23 |
|---|---|---|---|---|---|---|---|---|---|
| rules, constants only | 124 | 1332 | 9385 | 19795 | 57022 | 150040 | 195157 | 200953 | |
| time | 0.0 s | 0.0 s | 0.1 s | 0.2 s | 0.7 s | 2.1 s | 2.9 s | 3.0 s | |
| rules with variables | 70 | 399 | 1926 | 4874 | 31417 | 350487 | 1162845 | 1654065 | 1670537 |
| time | 0.0 s | 0.1 s | 0.4 s | 1.1 s | 6.9 s | 88 s | 8.2 min | 16.7 min | 17.3 min |

The 232 normal forms are the boolean functions of three inputs up to renaming of
the constants. The largest normal form has size 10, so left-hand sides end at size 21. Every
argument of an enumerated term is an injective renaming of a normal form (line 9 of Algorithm 1). Without variables no rule matches at the root of an
enumerated term (a constant-only rule matches only terms of its own size): each of the 201185 enumerated terms becomes a rule or a normal form.

With variables, terms whose smaller equivalents all use some variable more often
cannot be rewritten, as the KBO cannot orient the rule: `((a|b)&(c|(~b)))` equals
`(a^(b&(a^c)))`, which duplicates `a`. Such a term stays a normal form with
variables (its constant instances are reduced by constant-only rules), e.g.
`(~((a|(~b))&(a^(b^c))))` at size 11. Left-hand sides up to size 22 combine them, e.g. `(((a&(~b))|(c^(b^a)))&(~((c|(~b))&(c^(a^b))))) -> ((a^b)&(b^c))`
(Sections 3 and 5). Variable rules also match at the root of larger terms: of the
12.4 M enumerated terms 61.5 % are skipped by rules of smaller size (line 10 of
Algorithm 1), 25 % by rules of the
same size found earlier in the processing order, and 13.5 % become rules or normal
forms.

## `budget`: integer and bitvector theories within ten minutes (Section 6.6)

Each theory is synthesized for 10 minutes (`BUDGET=600`, memory capped at 10 GB by `MEMORY_KB`). Output: `out/synth/{int,bv4,bv32}_{v0c3,vcs3}.*`.

| system | largest completed size | rules | normal forms | time for the completed sizes |
|---|---|---|---|---|
| int, constants only | 13 | 2525881 | 519670 | 7.4 min |
| int, with variables | 12 | 19315 | 446233 | 2.0 min, then the memory cap |
| bv4, constants only | 8 | 232971 | 249943 | 7.1 min |
| bv4, with variables | 8 | 15123 | 890711 | 9.8 min |
| bv32, constants only | 5 | 585 | 721 | 16 s |
| bv32, with variables | 5 | 185 | 2254 | 27 s |

Equivalence is decided exhaustively for bv4 and with Z3 for int and bv32 (200 random inputs as a filter). Equivalences Z3 cannot decide within 1 s are counted as different. For bv32, size 6 does not finish within the budget, as Z3 is slow on multiplication at width 32.

**Note on bv4 with variables:** size 8 completes with little margin (586–595 s of
600 s). On a slower or loaded machine the run stops at size 7, and Fig. 8 then
lacks the point at size 8. 
`BUDGET=700 ./run.sh budget sweep` recomputes both.
`docker run --rm -u "$(id -u):$(id -g)" -e BUDGET=700 -v "$PWD/eval/out-docker:/eval/out" rule-enum-eval budget sweep` for docker.

## `coverage`: coverage against Ruler (Table 1)

Ruler synthesizes rules for the boolean theory with 2 to 6 iterations. Ruler's
`derive` checks with 5 egg iterations which rules of one system the other system
proves (our constants read as variables). Output: `out/ruler/bool_it*.json`,
`out/ruler/derive_it*_s*.json`.

| Ruler iterations | Ruler rules | Ruler time | our size | our rules | Ruler derives ours | ours derive Ruler |
|---|---|---|---|---|---|---|
| 2 | 18 | 0.02 s | 5 | 124 | 124/124 (100 %) | 18/18 (100 %) |
| 3 | 27 | 0.08 s | 7 | 1332 | 1330/1332 (99.85 %) | 27/27 (100 %) |
| 4 | 33 | 25 s (24–26) | 9 | 9385 | 9203/9385 (98.06 %) | 33/33 (100 %) |
| 5 | 33 | 132 s (129–137) | 9 | 9385 | 9203/9385 (98.06 %) | 33/33 (100 %) |
| 6 | 35 | 740 s (666–740) | – | – | – | – |

The experiment also checks the off-diagonal pairs, which show where derivability
fails: ours fail only on Ruler rules larger than our size cap, Ruler fails on ours
when its depth is too small.

| Ruler iterations | our size | Ruler derives ours | ours derive Ruler |
|---|---|---|---|
| 2 | 7 | 1212/1332 (90.99 %) | 18/18 (100 %) |
| 3 | 5 | 124/124 (100 %) | 24/27 (88.89 %) |
| 3 | 9 | 9151/9385 (97.51 %) | 27/27 (100 %) |
| 4 | 7 | 1330/1332 (99.85 %) | 32/33 (96.97 %) |

Each derivation has a wall-clock limit of 10 s in Ruler (`src/derive.rs`).  Ruler fails on 92 distinct left-hand sides of our
size-9 system (182 rules, as some coincide once constants are read as variables).
Ruler with 6 iterations needs about 20 GB of memory.

## `greedy500`: greedy simplification by system size (Fig. 3)

1000 random terms of size 500 over three constants, simplified greedily by the
size-5, size-9 and complete systems. Output: `out/results/greedy500.json`. Median
and maximum of the simplified sizes:

| system | constants only | with variables |
|---|---|---|
| size 5 | (423, 460) | (158.5, 338) |
| size 9 | (256, 359) | (5, 95) |
| complete | (4, 10) | (4, 10) |

## `ruler50`: greedy and e-graph simplification against Ruler (Fig. 5)

1000 random terms of size 50, simplified greedily (left) and in one e-graph per
term with 2 iterations of egglog (right). Output: `out/results/ruler50.json`.
Median and maximum:

| system | greedy | e-graph |
|---|---|---|
| Ruler, 2 iterations | (46, 50) | (36, 50) |
| Ruler, 4 iterations | (46, 50) | (34, 50) |
| ours, size 5 | (42, 50) | (30, 46) |
| ours, size 5, rules in both directions | – | (24, 42) |
| ours, size 9 | (23, 46) | – |

For greedy rewriting, Ruler's pattern variables are turned into constants (`?a`
to `A`) to avoid infinite cycles, so Ruler's rules fire only on leaves. The experiment also reports Ruler's
size-reducing rules applied with their variables, which reach medians 44 and 43
(maximum 50). On terms of size 50 egglog's results are stable: a few terms come
out as different terms of the same size, which leaves all statistics unchanged.

## `large`: large terms, greedy against e-graph (Fig. 4)

50 random terms per size from 50 to 3000, simplified greedily with the complete
system with variables and in an e-graph with Ruler's 2-iteration rules until 50000
e-nodes. Output: `out/results/large.json`, `out/results/large_times.csv`.

| input size | 50 | 100 | 250 | 500 | 1000 | 2000 | 3000 |
|---|---|---|---|---|---|---|---|
| greedy mean | 5.22 | 5.06 | 4.82 | 4.16 | 4.20 | 4.50 | 4.98 |
| greedy time | 0.01 s | 0.01 s | 0.03 s | 0.06 s | 0.12 s | 0.25 s | 0.39 s |
| e-graph mean (paper) | 9.44 | 11.9 | 20.2 | 26.9 | 57.4 | 52.4 | 235.4 |
| e-graph mean (range) | 9.44 | 11.9 | 20.2–20.9 | 25.8–27.8 | 56.1–60.0 | 45.1–52.4 | 235.4–257.5 |
| e-graph time | 87 s | 68 s | 45 s | 102 s | 42 s | 138 s | 44 s |

The greedy values are deterministic. The e-graph's times vary by about 10 %.

## `twee`: unfailing completion with Twee (Fig. 6)

Twee completes the axioms of `bool_complete_generator.p` with `--max-term-size N`.
Its rules and our size-N system with variables simplify 500 random terms of size 50
greedily. Output: `out/results/twee.json`, `out/results/twee_times.csv`.

| size | Twee rules | Twee mean | our mean | Twee time | our synthesis time |
|---|---|---|---|---|---|
| 5 | 14 | 46.03 | 23.10 | 0.04 s | 0.01 s |
| 7 | 92 | 36.93 | 8.79 | 0.14 s | 0.08 s |
| 9 | 820 (819–821) | 24.18 (24.18–24.21) | 5.70 | 45 s (34–45) | 0.43 s |

At size 9 Twee stops at its resource limit, and the system it reports differs
slightly between runs, in a few right-hand sides that differ by commutation.

## `sweep`: simplification by maximal rule size (Figs. 7, 8)

The size-N systems (left-hand sides up to size N) simplify random terms greedily:
bool on 100 terms of size 1000, int and bitvectors on 50 terms of size 500. Ruler's
rules simplify the same terms in one e-graph per term with 2 iterations, placed at
the size of their largest term. The int and bitvector systems are those of
`budget`. Output: `out/results/sweep_*.json`. Median simplified size:

| max rule size | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13 | 14 |
|---|---|---|---|---|---|---|---|---|---|---|---|---|
| bool, constants | 925 | 921 | 841 | 747.5 | 684 | 593 | 509.5 | 436.5 | 354 | 292.5 | 216.5 | 118 |
| bool, variables | 880.5 | 846.5 | 206 | 56.5 | 16.5 | 6 | 5 | 5 | 5 | 4 | 4 | 4 |
| int, constants | 480 | 467 | 441.5 | 427.5 | 411 | 405.5 | 392.5 | 386.5 | 381 | 376 | | |
| int, variables | 458 | 389 | 288 | 287 | 275 | 275 | 270 | 271 | 266 | 266 | | |
| bv4, constants | 480 | 475.5 | 461.5 | 452 | 444 | 440 | | | | | | |
| bv4, variables | 462 | 438 | 380.5 | 356 | 350 | 331 | | | | | | |
| bv32, constants | 480 | 475.5 | 461.5 | | | | | | | | | |
| bv32, variables | 462 | 438 | 384.5 | | | | | | | | | |

| Ruler in an e-graph | bool, 2 / 3 / 4 iterations | int, 2 | bv4, 2 / 3 | bv32, 2 |
|---|---|---|---|---|
| placed at rule size | 5 / 7 / 9 | 7 | 5 / 7 | 5 |
| paper | 429.5 / 308 / 293 | 438.5 | 430 / 416.5 | 436.5 |
| range | 429.5–444.5 / 296.5–309 / 293–302.5 | 437–438.5 | 429–431.5 / 416–417 | 436–436.5 |

The greedy values are deterministic. Ruler writes bitvector subtraction `--` and
integer negation `~`. `convert.py` translates these operator names.

## `noise`: run-to-run variation of the e-graph (not in the paper)

Ruler's bool rules in an e-graph on the 100 terms of the bool sweep. Output: `out/results/noise.json`. Median simplified size:

| Ruler iterations | range over 20 runs |
|---|---|
| 2 | 415–459 |
| 3 | 295–308 |
| 4 | 288–304 |

## `constants`: more constants in the terms than in the system (Section 6.7)

The paper names as a threat to validity that the guarantees hold only for the
number of constants of the signature. Systems for k constants (and k variables),
each of size 9, simplify 100 terms of size 1000 over 2 to 10 constants. Output:
`out/results/constants.json`. Median simplified size:

| system of size 9 | rules | normal forms | 2 constants | 3 constants | 5 constants | 10 constants |
|---|---|---|---|---|---|---|
| 2 constants | 925 | 14 | 3 | 665 | 806.5 | 882 |
| 3 constants | 9385 | 220 | 3 | 509.5 | 716 | 833.5 |
| 4 constants | 25933 | 3826 | 3 | 509.5 | 705.5 | 821.5 |
| 2 constants, 2 variables | 534 | 46 | 3 | 91.5 | 301 | 648 |
| 3 constants, 3 variables | 1926 | 1990 | 3 | 5 | 51 | 221.5 |
| 4 constants, 4 variables | 3251 | 31383 | 3 | 4.5 | 21 | 141.5 |

The 2-constant system is complete, hence also for 2-constant terms of any size.
Constant-only systems carry over to terms with more constants by the
order-preserving renaming, but only weakly: their rules match only where at most k
constants meet. Variable rules match regardless of the constants below them, so
systems with variables degrade gracefully.

## `validate`: correctness checks

The test suite passes all 68 checks (soundness, orientation, irreducibility and
ground completeness for bool, bv4, int with Z3 and demo). Both complete systems of
`synthesis` bring each of the 116232 ground terms up to size 8 to a smallest
equivalent, unique up to renaming of the constants a term does not depend on:

```
68 checks passed
bool_v0c3: terms 116232, classes 226, classes with non-minimal normal forms 0, with several normal forms 0
bool_vcs3: terms 116232, classes 226, classes with non-minimal normal forms 0, with several normal forms 0
```