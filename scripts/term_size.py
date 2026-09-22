"""Domain-general node-size of a term string.

size(t) = number of operator occurrences + number of leaf tokens, which equals
the OCaml `Types.size` for a fully-parenthesized term in any notation (prefix or
infix). Operators are the domain's symbols (mirrors termgen.BUILTINS); leaves are
maximal alphanumeric runs (var/hole/constant names). Parens, commas, whitespace
are ignored.

    from term_size import make_sizer
    size = make_sizer("bool")     # or "int", "bv", "bv4"
    size("(~(A^B))")  # -> 4
"""
import re

# Operator symbols per built-in domain (kept in sync with scripts/termgen.py).
DOMAIN_OPS = {
    "bool": ["~", "&", "|", "^"],
    "int":  ["+", "-", "*"],
    "bv":   ["~", "-", "+", "*", "&", "|", "<<", ">>"],
}
DOMAIN_OPS["bv4"] = DOMAIN_OPS["bv"]
DOMAIN_OPS["bv32"] = DOMAIN_OPS["bv"]


def make_sizer(domain):
    # any bv<N> width shares the bv operator set
    ops = DOMAIN_OPS.get(domain) or (DOMAIN_OPS["bv"] if domain.startswith("bv") else None)
    if ops is None:
        raise KeyError(f"unknown domain {domain!r}")
    ops = sorted(set(ops), key=len, reverse=True)  # longest first
    pat = re.compile("(?:" + "|".join(re.escape(o) for o in ops) + r")|[A-Za-z0-9]+")
    return lambda s: len(pat.findall(s))
