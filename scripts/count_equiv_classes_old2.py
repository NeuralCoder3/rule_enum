from math import comb

# D(m): boolean functions of m variables that essentially depend on ALL m
# (inclusion-exclusion removing functions with any fictitious variable).
def essential(m):
    return sum((-1)**j * comb(m, j) * 2**(2**(m - j)) for j in range(m + 1))

# Holes renumber to the smallest-id prefix preserving order, so each behavior
# class maps to the prefix of holes it essentially depends on.
def num_irreducibles(k):
    return sum(essential(m) for m in range(k + 1))

for k in range(5):
    print(f"k={k}: per-prefix {[essential(m) for m in range(k+1)]} "
          f"-> {num_irreducibles(k)} irreducibles")
# k=2 -> 14, k=3 -> 232, k=4 -> 64826


# import itertools

# var_set_1 = ['a', 'b', 'c']
# # var_set_1 = ['a', 'b']
# # var_set_2 = ['A', 'B', 'C']

# # all subsets
# # for subset in itertools.chain.from_iterable(itertools.combinations(var_set_1, r) for r in range(len(var_set_1)+1)):
# #     print(subset)

# # prefixes of set
# classes = []
# for i in range(len(var_set_1)+1):
#     prefix = var_set_1[:i]
#     # print(prefix)
#     for assignment in itertools.product([0, 1], repeat=len(prefix)+1):
#         # print(assignment)
#         classes.append((prefix, assignment))
        
# print("Number of equivalence classes:", len(classes))


# # (~(A&B))
# # (~(A|B))
# # (~(A^A))
# # (~(A^B))
# # (A&(~B))
# # (B&(~A))
# # (A|(~B))
# # (B|(~A))
# # (A&B)
# # (A|B)
# # (A^A)
# # (A^B)
# # (~A)
# # A
