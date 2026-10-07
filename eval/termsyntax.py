"""Terms as ('leaf', name) or (op, [children]) in three syntaxes: ours
(infix (a&b), unary (~a), applied <<(a,b), constants 0()), the prefix
benchmark syntax &(x, y), and Ruler's s-expressions (& ?a ?b)."""
import re

TOKEN = re.compile(r"\s*(\?\w+|[A-Za-z]\d*|\d+|<<|>>|[~&|^+*\-]|[(),])")


def tokens(text):
    out, pos = [], 0
    while pos < len(text.rstrip()):
        m = TOKEN.match(text, pos)
        if not m:
            raise ValueError(f"cannot tokenize {text!r} at {pos}")
        out.append(m.group(1))
        pos = m.end()
    return out


def is_leaf(tok):
    return tok[0].isalpha() or tok[0] == "?"


def parse(text):
    toks, i = tokens(text), 0

    def term():
        nonlocal i
        tok = toks[i]
        i += 1
        if is_leaf(tok):
            return ("leaf", tok)
        if tok == "(":
            if is_leaf(toks[i]) or toks[i] == "(" or (toks[i + 1] == "(" and toks[i] not in "~-"):
                lhs = term()
                op = toks[i]
                i += 1
                rhs = term()
                i += 1
                return (op, [lhs, rhs])
            op = toks[i]
            i += 1
            arg = term()
            i += 1
            return (op, [arg])
        args = []
        i += 1
        while toks[i] != ")":
            args.append(term())
            if toks[i] == ",":
                i += 1
        i += 1
        return (tok, args)

    return term()


RULER_GLYPHS = {"bv": {"--": "-"}, "int": {"~": "-"}}


def from_ruler(t, domain):
    glyphs = RULER_GLYPHS.get(domain.rstrip("0123456789"), {})
    return t if t[0] == "leaf" else (glyphs.get(t[0], t[0]), [from_ruler(a, domain) for a in t[1]])


def parse_sexpr(text):
    toks, i = re.findall(r"[()]|[^\s()]+", text), 0

    def term():
        nonlocal i
        tok = toks[i]
        i += 1
        if tok != "(":
            return ("leaf", tok)
        op, args = toks[i], []
        i += 1
        while toks[i] != ")":
            args.append(term())
        i += 1
        return (op, args)

    return term()


def infix(t):
    op, args = t
    if op == "leaf":
        return args
    if len(op) == 1 and len(args) == 1:
        return f"({op}{infix(args[0])})"
    if len(op) == 1 and len(args) == 2:
        return f"({infix(args[0])}{op}{infix(args[1])})"
    return op + "(" + ",".join(infix(a) for a in args) + ")"


def sexpr(t):
    op, args = t
    if op == "leaf":
        return args
    return "(" + " ".join([op] + [sexpr(a) for a in args]) + ")"


def size(t):
    return 1 if t[0] == "leaf" else 1 + sum(size(a) for a in t[1])


def leaves(t):
    return {t[1]} if t[0] == "leaf" else set().union(*map(leaves, t[1]))


def rename(t, f):
    return ("leaf", f(t[1])) if t[0] == "leaf" else (t[0], [rename(a, f) for a in t[1]])


def text_size(text):
    return sum(tok not in "()," for tok in tokens(text))
