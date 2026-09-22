#!/usr/bin/env python3
"""Minimize a boolean `.rules` file with the algebra minimizer (twee).

Pipeline (intermediate files land in ``eval/min_rules/``):

  1. read the infix ``.rules`` file (e.g. ``eval/bool_v0c3_s12.rules``);
  2. convert each rule to twee/TPTP prefix form with function names
     (like ``scripts/algebra_minimizer/rules.txt``), writing ``<base>.txt``;
  3. minimize with ``scripts/algebra_minimizer/minimizer_greedy_top_down.py``,
     writing the kept set to ``<base>.min.txt`` (redundant -> ``<base>.redundant.txt``);
  4. convert the kept rules back to the infix ``.rules`` format, ``<base>.min.rules``;
  5. convert that to Ruler's rules JSON, ``<base>.min.json``.

The minimizer drives twee inside a docker container (see
``scripts/algebra_minimizer/twee.sh``); this script starts that container if
needed.

Boolean operator <-> function-name mapping:
    ~ <-> not    & <-> and    ^ <-> xor    | <-> or

Usage:
    python scripts/minimize_rules.py eval/bool_v0c3_s12.rules
    python scripts/minimize_rules.py eval/bool_v0c3_s12.rules --timeout 2
"""

import argparse
import os
import subprocess
import sys

# Make sibling scripts importable (rules_to_ruler.py lives next to this file).
SCRIPT_DIR = os.path.dirname(os.path.abspath(__file__))
ALGEBRA_DIR = os.path.join(SCRIPT_DIR, "algebra_minimizer")
sys.path.insert(0, SCRIPT_DIR)

# Reuse the existing infix parser / AST and the .rules -> Ruler JSON converter.
from rules_to_ruler import PrattParser, ASTNode, process_rules  # noqa: E402

# Boolean logic operators with their precedence (same table rules_to_ruler.py
# uses) and the function names expected by the algebra minimizer.
LOGIC_SYMBOLS = [
    ("~", 1, 40),
    ("&", 2, 30),
    ("^", 2, 20),
    ("|", 2, 10),
]
OP_TO_FUNC = {"~": "not", "&": "and", "^": "xor", "|": "or"}
FUNC_TO_OP = {v: k for k, v in OP_TO_FUNC.items()}


# --------------------------------------------------------------------------- #
# infix .rules  ->  prefix function form                                       #
# --------------------------------------------------------------------------- #

def ast_to_prefix(node):
    """Render an ASTNode (from PrattParser) as ``and(A,B)`` prefix form."""
    if not node.args:
        # Leaf: variable (uppercase A,B,C) or object var / constant (lowercase).
        return node.symbol
    func = OP_TO_FUNC.get(node.symbol, node.symbol)
    return f"{func}({','.join(ast_to_prefix(a) for a in node.args)})"


def rules_to_prefix(in_path, out_path):
    """Read an infix .rules file, write twee prefix equations (lhs = rhs)."""
    parser = PrattParser(LOGIC_SYMBOLS)
    out_lines = []
    with open(in_path) as f:
        for line_num, line in enumerate(f, 1):
            line = line.strip()
            if not line:
                continue
            if "<->" in line:
                left, right = line.split("<->")
            elif "->" in line:
                left, right = line.split("->")
            else:
                print(f"Skipping line {line_num}: no arrow found.", file=sys.stderr)
                continue
            lhs = ast_to_prefix(parser.parse(left))
            rhs = ast_to_prefix(parser.parse(right))
            out_lines.append(f"{lhs} = {rhs}")
    with open(out_path, "w") as f:
        f.write("\n".join(out_lines) + ("\n" if out_lines else ""))
    return out_lines


# --------------------------------------------------------------------------- #
# prefix function form  ->  infix .rules                                       #
# --------------------------------------------------------------------------- #

def parse_prefix(text):
    """Parse ``and(A,B)`` style prefix term into a nested ('op'|'leaf', ...) AST."""
    pos = 0

    def skip_ws():
        nonlocal pos
        while pos < len(text) and text[pos].isspace():
            pos += 1

    def parse_term():
        nonlocal pos
        skip_ws()
        start = pos
        while pos < len(text) and (text[pos].isalnum() or text[pos] == "_"):
            pos += 1
        name = text[start:pos]
        if not name:
            raise ValueError(f"expected identifier at {start} in {text!r}")
        skip_ws()
        if pos < len(text) and text[pos] == "(":
            pos += 1  # consume '('
            args = [parse_term()]
            skip_ws()
            while pos < len(text) and text[pos] == ",":
                pos += 1
                args.append(parse_term())
                skip_ws()
            if pos >= len(text) or text[pos] != ")":
                raise ValueError(f"expected ')' in {text!r}")
            pos += 1  # consume ')'
            return ("op", name, args)
        return ("leaf", name)

    term = parse_term()
    skip_ws()
    if pos != len(text):
        raise ValueError(f"trailing tokens in {text!r}: {text[pos:]!r}")
    return term


def prefix_ast_to_infix(node):
    """Render a parse_prefix AST as fully-parenthesised infix (.rules form)."""
    if node[0] == "leaf":
        return node[1]
    _, name, args = node
    op = FUNC_TO_OP.get(name)
    if op is None:
        raise ValueError(f"unknown function name {name!r}")
    if len(args) == 1:
        return f"({op}{prefix_ast_to_infix(args[0])})"
    if len(args) == 2:
        return f"({prefix_ast_to_infix(args[0])}{op}{prefix_ast_to_infix(args[1])})"
    raise ValueError(f"function {name!r} has arity {len(args)}; "
                     "only unary/binary supported by .rules")


def prefix_to_rules(in_path, out_path):
    """Read twee prefix equations, write infix .rules (lhs -> rhs)."""
    out_lines = []
    with open(in_path) as f:
        for line in f:
            line = line.strip()
            if not line:
                continue
            # Equations use '='; orient left -> right for the .rules file.
            lhs, rhs = line.split("=", 1)
            lhs_infix = prefix_ast_to_infix(parse_prefix(lhs))
            rhs_infix = prefix_ast_to_infix(parse_prefix(rhs))
            out_lines.append(f"{lhs_infix} -> {rhs_infix}")
    with open(out_path, "w") as f:
        f.write("\n".join(out_lines) + ("\n" if out_lines else ""))
    return out_lines


# --------------------------------------------------------------------------- #
# twee docker container                                                        #
# --------------------------------------------------------------------------- #

def ensure_twee_container(name="twee-container", image="neuralcoder/twee:latest"):
    """Make sure the twee docker container the minimizer talks to is running."""
    def docker(*args):
        return subprocess.run(["docker", *args], capture_output=True, text=True)

    running = docker("ps", "--filter", f"name=^{name}$", "--format", "{{.Names}}")
    if running.returncode == 0 and name in running.stdout.split():
        return
    exists = docker("ps", "-a", "--filter", f"name=^{name}$", "--format", "{{.Names}}")
    if exists.returncode == 0 and name in exists.stdout.split():
        print(f"Starting existing twee container '{name}'...", file=sys.stderr)
        docker("start", name)
    else:
        print(f"Creating twee container '{name}' from {image}...", file=sys.stderr)
        docker("run", "-d", "--name", name, image, "tail", "-f", "/dev/null")


# --------------------------------------------------------------------------- #
# minimization (runs in the algebra_minimizer dir so ./twee.sh resolves)       #
# --------------------------------------------------------------------------- #

def run_minimizer(prefix_rules, timeout):
    """Call minimizer_greedy_top_down.minimize, returns (kept, redundant)."""
    sys.path.insert(0, ALGEBRA_DIR)
    cwd = os.getcwd()
    os.chdir(ALGEBRA_DIR)  # twee.sh is referenced relatively by common.run_twee
    try:
        import minimizer_greedy_top_down as mz
        return mz.minimize(prefix_rules, timeout=timeout)
    finally:
        os.chdir(cwd)


# --------------------------------------------------------------------------- #
# main                                                                         #
# --------------------------------------------------------------------------- #

def main(argv=None):
    p = argparse.ArgumentParser(
        description="Minimize a boolean .rules file with the algebra minimizer.",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog=__doc__,
    )
    p.add_argument("rules", help="input .rules file (e.g. eval/bool_v0c3_s12.rules)")
    p.add_argument("--outdir", default=None,
                   help="intermediate/output directory (default: eval/min_rules)")
    p.add_argument("--timeout", type=int, default=1,
                   help="twee timeout per implication check, in seconds (default 1)")
    p.add_argument("--no-docker", action="store_true",
                   help="don't try to start the twee docker container")
    args = p.parse_args(argv)

    repo_root = os.path.dirname(SCRIPT_DIR)
    outdir = args.outdir or os.path.join(repo_root, "eval", "min_rules")
    os.makedirs(outdir, exist_ok=True)

    base = os.path.splitext(os.path.basename(args.rules))[0]
    prefix_path = os.path.join(outdir, base + ".txt")
    min_prefix_path = os.path.join(outdir, base + ".min.txt")
    redundant_path = os.path.join(outdir, base + ".redundant.txt")
    min_rules_path = os.path.join(outdir, base + ".min.rules")
    min_json_path = os.path.join(outdir, base + ".min.json")

    # 1+2. infix .rules -> prefix function form
    prefix_rules = rules_to_prefix(args.rules, prefix_path)
    print(f"[1/4] {len(prefix_rules)} rules -> prefix form: {prefix_path}",
          file=sys.stderr)

    # 3. minimize
    if not args.no_docker:
        ensure_twee_container()
    kept, redundant = run_minimizer(prefix_rules, timeout=args.timeout)
    with open(min_prefix_path, "w") as f:
        f.write("\n".join(kept) + ("\n" if kept else ""))
    with open(redundant_path, "w") as f:
        f.write("\n".join(redundant) + ("\n" if redundant else ""))
    print(f"[2/4] minimized: kept {len(kept)}, redundant {len(redundant)} "
          f"-> {min_prefix_path}", file=sys.stderr)

    # 4. prefix -> infix .rules
    min_rules = prefix_to_rules(min_prefix_path, min_rules_path)
    print(f"[3/4] back to .rules: {len(min_rules)} rules -> {min_rules_path}",
          file=sys.stderr)

    # 5. .rules -> Ruler JSON
    process_rules(min_rules_path, min_json_path)
    print(f"[4/4] Ruler JSON: {min_json_path}", file=sys.stderr)


if __name__ == "__main__":
    main()
