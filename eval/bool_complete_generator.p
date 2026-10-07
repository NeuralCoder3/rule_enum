% ============================================================================
%  Axis A (generator) -- run twee as a Knuth-Bendix completion tool and
%  compare the system it produces against RuleGen's.
%
%  twee always runs completion while searching for a proof.
%
%  Run (budget it):
%    twee bool_complete_generator.p --max-term-size 9
%
%  Expected: completion does not terminate. The AC
%  axioms (associativity + commutativity of and, or, xor) generate unbounded
%  critical pairs, so twee runs to the budget without saturating. 
% ============================================================================

cnf(or_commutative,  axiom, or(X,Y)         = or(Y,X)).
cnf(and_commutative, axiom, and(X,Y)        = and(Y,X)).
cnf(or_associative,  axiom, or(or(X,Y),Z)   = or(X,or(Y,Z))).
cnf(and_associative, axiom, and(and(X,Y),Z) = and(X,and(Y,Z))).
cnf(or_identity,     axiom, or(X,zero)      = X).
cnf(and_identity,    axiom, and(X,one)      = X).
cnf(or_distributes,  axiom, or(X,and(Y,Z))  = and(or(X,Y),or(X,Z))).
cnf(and_distributes, axiom, and(X,or(Y,Z))  = or(and(X,Y),and(X,Z))).
cnf(or_complement,   axiom, or(X,not(X))    = one).
cnf(and_complement,  axiom, and(X,not(X))   = zero).
cnf(xor_definition,  axiom, xor(X,Y)        = or(and(X,not(Y)),and(not(X),Y))).

% Non-theorem goal: forces completion to run to the resource budget.
cnf(force_completion, negated_conjecture, a != b).
