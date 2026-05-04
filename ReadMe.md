Iteration n:
Enumerate terms of size n from previous irreducible terms with variable placeholder (replace variable leafs by these placeholders).
Instantiate the variables such that the first occurence of a variable is lexicographic and no variable is skipped (e.g. enum a-b+b, a-b+a, but not b+a-a or b+c-c).

Simplify with previous rule set. If the size gets smaller, discard (if a size reducing rule was applied).
On term, get smallest KBO equivalent term. 
If it is size-smaller, add new rule to size reducing rules.
If it is KBO-smaller, add new rule to KBO simplifying rules.
If it is the term itself, add to irreducible terms. 


Keep rules in two sets: size reducing, only kbo simplifying.
Keep irreducible terms in a set.


To apply rules, we look at renamings. E.g. a-b+b -> a also applied in a*(b-c+c) -> a*b.

To synthesize the minimal KBO term of a term t, we have 100 pre-determined random inputs. (Not with SMT synthesis as written in the proof)
For each irreducibile term, we keep a list of the result on these inputs. 
If the term t behaves equivalent to a previous (smaller) irreducible term on all inputs, we return the smaller term (and the algorithm will add a size-reducing rule).
If it differs to all previous irreducibile term, we keep it for now.
After an iteration, we have all terms that are not equivalent to a smaller term.
We group them by their example behavior. If there are multiple terms with the same behavior, we keep the smallest one (according to KBO) and add rules to reduce the others to it (KBO-simplifying rules).


Next steps:
New inputs (smt counter example) to distinguish terms.