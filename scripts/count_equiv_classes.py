from math import comb

# D(m): boolean functions of m variables that essentially depend on ALL m
# (inclusion-exclusion removing functions with any fictitious variable).
def essential(m):
    return sum((-1)**j * comb(m, j) * 2**(2**(m - j)) for j in range(m + 1))

# --- holes only -------------------------------------------------------------
# Holes renumber to the smallest-id prefix preserving order, so each behavior
# class maps to the prefix of holes it essentially depends on.  EXACT.
def num_irreducibles(k):
    return sum(essential(m) for m in range(k + 1))

for k in range(5):
    print(f"holes-only k={k}: per-prefix {[essential(m) for m in range(k+1)]} "
          f"-> {num_irreducibles(k)} irreducibles")
# k=2 -> 14, k=3 -> 232, k=4 -> 64826

# --- vars + holes (the --max-vcs k setting, v + h <= k) ---------------------
# A behavior uses v vars (a,b,..) and h holes (A,B,..).  KEY facts:
#   * Both vars and holes canonicalize to a PREFIX (vars by first-occurrence,
#     holes by sorted-id rank).
#   * Neither is permutation-merged: `a&~b` and `~a&b` are DISTINCT behaviors
#     (canonicalization renames labels but never reorders operands).  So vars
#     and holes are just distinct, ordered, TYPED input dimensions.
# Hence a (v,h) split contributes E(v+h) behaviors (functions essentially
# depending on the v-var-prefix + h-hole-prefix), and there are (n+1) splits
# with v+h = n:
#         N(k) = sum_{n=0}^{k} (n+1) * E(n)
def num_irreducibles_vars(k):
    return sum((n + 1) * essential(n) for n in range(k + 1))

print()
for k in range(1, 4):
    print(f"vars+holes k={k} (v+h<={k}): "
          f"per-n (n+1)*E(n) {[(n+1)*essential(n) for n in range(k+1)]} "
          f"-> {num_irreducibles_vars(k)}")
# Validated against the synthesis with --one-per-class:
#   k=1 -> 6   (exact)
#   k=2 -> 36  (exact)
#   k=3 -> 908 (minimal/ideal count)
#
# CAVEAT for k>=3: the real synthesis keeps a little MORE than this lower bound
# because it retains
#   (a) irreducibles with a DEAD (fictitious) variable -> non-prefix support
#       (~31 extra at k=3), and
#   (b) in the default mode, KBO-incomparable commutativity variants
#       (A&a / a&A, ...; run with --one-per-class to drop these).
# So N(k) is the minimal behavior-class count; the synthesis converges a bit
# above it for k>=3.
