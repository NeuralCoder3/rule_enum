"""Ground-completeness check for bool rule sets via a binary's --eval mode.

usage: ground_check.py gen OUT N          write all ground terms over A,B,C up to size N
       ground_check.py check TERMS NORMS  per truth table: minimal size, unique up to renaming
"""
import itertools, re, sys

OPS = {'&': lambda a, b: a and b, '|': lambda a, b: a or b, '^': lambda a, b: a != b}


def gen(n, memo={}):
    if n in memo:
        return memo[n]
    out = ['A', 'B', 'C'] if n == 1 else []
    if n >= 2:
        out += ['(~%s)' % t for t in gen(n - 1)]
        for i in range(1, n - 1):
            for l in gen(i):
                for r in gen(n - 1 - i):
                    out += ['(%s%s%s)' % (l, o, r) for o in OPS]
    memo[n] = out
    return out


def parse(s):
    pos = 0

    def term():
        nonlocal pos
        c = s[pos]
        if c.isalpha():
            pos += 1
            return ('leaf', c.upper())
        pos += 1  # '('
        if s[pos] == '~':
            pos += 1
            a = term()
            pos += 1
            return ('~', a)
        a = term()
        op = s[pos]
        pos += 1
        b = term()
        pos += 1
        return (op, a, b)

    return term()


def ev(t, env):
    if t[0] == 'leaf':
        return env[t[1]]
    if t[0] == '~':
        return not ev(t[1], env)
    return OPS[t[0]](ev(t[1], env), ev(t[2], env))


def size(t):
    return 1 if t[0] == 'leaf' else 1 + sum(size(x) for x in t[1:])


def table(t):
    return tuple(ev(t, dict(zip('ABC', v))) for v in itertools.product([False, True], repeat=3))


def canonical(s):
    letters = sorted(set(re.findall('[A-Z]', s)))
    return re.sub('[A-Z]', lambda m: 'ABCDEFGHIJ'[letters.index(m.group())], s)


if sys.argv[1] == 'gen':
    with open(sys.argv[2], 'w') as f:
        for n in range(1, int(sys.argv[3]) + 1):
            f.write('\n'.join(gen(n)) + '\n')
else:
    terms = [l.strip() for l in open(sys.argv[2]) if l.strip()]
    norms = [l.strip() for l in open(sys.argv[3]) if l.strip()]
    classes = {}
    for t, n in zip(terms, norms):
        pt, pn = parse(t), parse(n)
        assert table(pt) == table(pn), ('unsound', t, n)
        classes.setdefault(table(pt), []).append((size(pt), n, size(pn)))
    not_minimal = not_unique = 0
    examples = []
    for members in classes.values():
        smallest = min(s for s, _, _ in members)
        bad = [(n, s) for _, n, s in members if s != smallest]
        if bad:
            not_minimal += 1
            examples.append('min size %d, reached %s' % (smallest, sorted(set(bad))[:3]))
        if len({canonical(n) for _, n, _ in members}) > 1:
            not_unique += 1
    print('terms %d, classes %d, classes with non-minimal normal forms %d, with several normal forms %d'
          % (len(terms), len(classes), not_minimal, not_unique))
    for e in examples[:4]:
        print('  ', e)
