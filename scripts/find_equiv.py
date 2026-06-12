# term = ""

import itertools
import re

class LogicVar:
    """
    A custom wrapper for booleans that overloads Python bitwise operators.
    This allows us to evaluate string expressions directly.
    """
    def __init__(self, val):
        self.val = bool(val)
        
    def __and__(self, other): 
        return LogicVar(self.val and other.val)
        
    def __or__(self, other):  
        return LogicVar(self.val or other.val)
        
    def __xor__(self, other): 
        return LogicVar(self.val != other.val)
        
    def __invert__(self):     
        return LogicVar(not self.val)

def get_truth_table(expression):
    """
    Generates a tuple representing the truth table for a given expression.
    Tests all 8 possible combinations of a, b, c.
    """
    # Convert '!' to '~' so Python can use our overloaded __invert__
    py_expr = expression.replace('!', '~')
    
    truth_table = []
    # Test all True/False combinations for (a, b, c)
    for a, b, c in itertools.product([False, True], repeat=3):
        env = {
            'a': LogicVar(a),
            'b': LogicVar(b),
            'c': LogicVar(c)
        }
        # Evaluate safely without builtins
        result = eval(py_expr, {"__builtins__": None}, env)
        truth_table.append(result.val)
        
    return tuple(truth_table)

def parse_expressions(text_list):
    """Parses the raw text list into a list of tuples: (size, expression)"""
    expressions = []
    for line in text_list.strip().split('\n'):
        # Matches patterns like "[size 5] (a&(b^c))"
        match = re.search(r'\[size\s+(\d+)\]\s+(.+)', line.strip())
        if match:
            size = int(match.group(1))
            expr = match.group(2).strip()
            expressions.append((size, expr))
    return expressions

def find_equivalent(target, raw_list):
    """Finds all expressions in the list that logically match the target."""
    print(f"Target Expression: {target}\n")
    
    target_table = get_truth_table(target)
    expressions = parse_expressions(raw_list)
    
    matches = []
    for size, expr in expressions:
        if get_truth_table(expr) == target_table:
            matches.append((size, expr))
            
    if not matches:
        print("No equivalent expressions found in the list.")
    else:
        print("Found Equivalent Expressions (Shortest First):")
        # Sort by size to show the simplest matching term at the top
        matches.sort(key=lambda x: x[0])
        for size, expr in matches:
            print(f" -> [size {size}] {expr}")
    return matches

# ==========================================
# Example Usage
# ==========================================

# 1. Your target expression
target_expr = "(a&b)|(!a&c)"

# 2. Paste your raw text data here
raw_expression_list = """
[size 1] a
[size 2] (!a)
[size 3] (a&b)
[size 3] (a|b)
[size 3] (a^a)
[size 3] (a^b)
[size 4] (!(a&b))
[size 4] (!(a|b))
[size 4] (!(a^a))
[size 4] (!(a^b))
[size 4] (a&(!b))
[size 4] ((!a)&b)
[size 4] (a|(!b))
[size 4] ((!a)|b)
[size 5] (a&(b&c))
[size 5] (a&(b|c))
[size 5] (a&(b^c))
[size 5] ((a|b)&b)
[size 5] ((a|b)&c)
[size 5] ((a^b)&c)
[size 5] (a|(b&c))
[size 5] (a|(b|c))
[size 5] (a|(b^c))
[size 5] ((a&b)|c)
[size 5] ((a^b)|c)
[size 5] (a^(b&c))
[size 5] (a^(b|c))
[size 5] (a^(b^c))
[size 5] ((a&b)^c)
[size 5] ((a|b)^c)
[size 6] (!(a&(b&c)))
[size 6] (!(a&(b|c)))
[size 6] (!(a&(b^c)))
[size 6] (!((a|b)&b))
[size 6] (!((a|b)&c))
[size 6] (!((a^b)&c))
[size 6] (!(a|(b&c)))
[size 6] (!(a|(b|c)))
[size 6] (!(a|(b^c)))
[size 6] (!((a&b)|c))
[size 6] (!((a^b)|c))
[size 6] (!(a^(b&c)))
[size 6] (!(a^(b|c)))
[size 6] (!(a^(b^c)))
[size 6] (!((a&b)^c))
[size 6] (!((a|b)^c))
[size 6] (a&(!(b&c)))
[size 6] (a&(!(b|c)))
[size 6] (a&(!(b^c)))
[size 6] (a&(b&(!c)))
[size 6] (a&((!b)&c))
[size 6] (a&(b|(!c)))
[size 6] (a&((!b)|c))
[size 6] ((!a)&(b&c))
[size 6] ((!a)&(b|c))
[size 6] ((!a)&(b^c))
[size 6] ((a|b)&(!c))
[size 6] ((a^b)&(!c))
[size 6] ((!(a&b))&c)
[size 6] ((!(a|b))&c)
[size 6] ((!(a^b))&c)
[size 6] ((a|(!b))&c)
[size 6] (((!a)|b)&c)
[size 6] (a|(!(b&c)))
[size 6] (a|(!(b|c)))
[size 6] (a|(!(b^c)))
[size 6] (a|(b&(!c)))
[size 6] (a|((!b)&c))
[size 6] (a|(b|(!c)))
[size 6] (a|((!b)|c))
[size 6] ((!a)|(b&c))
[size 6] ((!a)|(b|c))
[size 6] ((!a)|(b^c))
[size 6] ((a&b)|(!c))
[size 6] ((a^b)|(!c))
[size 6] ((!(a&b))|c)
[size 6] ((!(a|b))|c)
[size 6] ((!(a^b))|c)
[size 6] ((a&(!b))|c)
[size 6] (((!a)&b)|c)
[size 6] (a^(b&(!c)))
[size 6] (a^((!b)&c))
[size 6] (a^(b|(!c)))
[size 6] (a^((!b)|c))
[size 6] ((a&(!b))^c)
[size 6] (((!a)&b)^c)
[size 6] ((a|(!b))^c)
[size 6] (((!a)|b)^c)
[size 7] (!(a&((!b)&c)))
[size 7] (!(a&(b|(!c))))
[size 7] (!(a&((!b)|c)))
[size 7] (!((a|(!b))&c))
[size 7] (!(((!a)|b)&c))
[size 7] (!(a|(b&(!c))))
[size 7] (!(a|((!b)&c)))
[size 7] (!(a|((!b)|c)))
[size 7] (!((a&(!b))|c))
[size 7] (!(((!a)&b)|c))
[size 7] (a&((a|b)&c))
[size 7] (a&((a|b)^c))
[size 7] ((a|b)&(b&c))
[size 7] ((a|b)&(b|c))
[size 7] ((a|b)&(a^c))
[size 7] ((a|b)&(b^c))
[size 7] ((a^b)&(a|c))
[size 7] ((a^b)&(b|c))
[size 7] ((a^b)&(a^c))
[size 7] ((a^b)&(b^c))
[size 7] ((a|(b&c))&b)
[size 7] ((a|(b|c))&c)
[size 7] ((a|(b^c))&b)
[size 7] (((a^b)|c)&b)
[size 7] ((a^(b&c))&b)
[size 7] ((a^(b|c))&c)
[size 7] ((a^(b^c))&b)
[size 7] (((a|b)^c)&b)
[size 7] (a|((b|c)&c))
[size 7] ((a&b)|(b|c))
[size 7] ((a&b)|(a^c))
[size 7] ((a&b)|(b^c))
[size 7] ((a^a)|(b^c))
[size 7] ((a^b)|(a&c))
[size 7] ((a^b)|(a^c))
[size 7] ((a^(b|c))|b)
[size 7] (a^(b&(a^c)))
[size 7] (a^((b|c)&c))
[size 7] (a^((a^b)&c))
[size 7] (a^(b|(a&c)))
[size 7] (a^(b|(a^c)))
[size 7] (a^((a&b)|c))
[size 7] (a^((a^b)|c))
[size 7] (a^(b^(a&c)))
[size 7] (a^(b^(a|c)))
[size 7] ((a&b)^(a|c))
[size 7] ((a|b)^(a&c))
[size 7] ((a|b)^(b&c))
[size 7] ((a|b)^(b|c))
[size 7] ((a&(b&c))^b)
[size 7] ((a&(b|c))^c)
[size 7] ((a&(b^c))^b)
[size 7] ((a&(b^c))^c)
[size 7] (((a^b)&c)^b)
[size 7] ((a|(b&c))^b)
[size 7] ((a|(b&c))^c)
[size 7] ((a|(b|c))^b)
[size 7] (((a&b)|c)^b)
[size 7] (((a^b)|c)^b)
[size 8] (!(a&((a|b)&c)))
[size 8] (!(a&((a|b)^c)))
[size 8] (!((a|b)&(b&c)))
[size 8] (!((a|b)&(b|c)))
[size 8] (!((a|b)&(a^c)))
[size 8] (!((a|b)&(b^c)))
[size 8] (!((a^b)&(a|c)))
[size 8] (!((a^b)&(b|c)))
[size 8] (!((a^b)&(a^c)))
[size 8] (!((a^b)&(b^c)))
[size 8] (!((a|(b&c))&b))
[size 8] (!((a|(b|c))&c))
[size 8] (!((a|(b^c))&b))
[size 8] (!(((a^b)|c)&b))
[size 8] (!((a^(b&c))&b))
[size 8] (!((a^(b|c))&c))
[size 8] (!((a^(b^c))&b))
[size 8] (!(((a|b)^c)&b))
[size 8] (!(a|((b|c)&c)))
[size 8] (!((a&b)|(b|c)))
[size 8] (!((a&b)|(a^c)))
[size 8] (!((a&b)|(b^c)))
[size 8] (!((a^a)|(b^c)))
[size 8] (!((a^b)|(a&c)))
[size 8] (!((a^b)|(a^c)))
[size 8] (!((a^(b|c))|b))
[size 8] (!(a^(b&(a^c))))
[size 8] (!(a^((b|c)&c)))
[size 8] (!(a^((a^b)&c)))
[size 8] (!(a^(b|(a&c))))
[size 8] (!(a^(b|(a^c))))
[size 8] (!(a^((a&b)|c)))
[size 8] (!(a^((a^b)|c)))
[size 8] (!(a^(b^(a&c))))
[size 8] (!(a^(b^(a|c))))
[size 8] (!((a&b)^(a|c)))
[size 8] (!((a|b)^(a&c)))
[size 8] (!((a|b)^(b&c)))
[size 8] (!((a|b)^(b|c)))
[size 8] (!((a&(b&c))^b))
[size 8] (!((a&(b|c))^c))
[size 8] (!((a&(b^c))^b))
[size 8] (!((a&(b^c))^c))
[size 8] (!(((a^b)&c)^b))
[size 8] (!((a|(b&c))^b))
[size 8] (!((a|(b&c))^c))
[size 8] (!((a|(b|c))^b))
[size 8] (!(((a&b)|c)^b))
[size 8] (!(((a^b)|c)^b))
[size 8] ((a|b)&(!(a^c)))
[size 8] ((a|b)&(!(b^c)))
[size 8] ((a|b)&(b|(!c)))
[size 8] ((!(a^b))&(a|c))
[size 8] ((!(a^b))&(a^c))
[size 8] ((a|(!b))&(b|c))
[size 8] ((a|(!b))&(b^c))
[size 8] (((!a)|b)&(b|c))
[size 8] (((!a)|b)&(b^c))
[size 8] (((a&b)|c)&(!b))
[size 8] ((a&b)|(b|(!c)))
[size 8] ((a^b)|(!(a|c)))
[size 8] ((a^b)|(!(a^c)))
[size 8] ((a^b)|(a&(!c)))
[size 8] ((a^b)|(c&(!a)))
[size 8] ((!(a|b))|(a^c))
[size 8] ((!(a|b))|(b^c))
[size 8] ((a&(!b))|(a^c))
[size 8] ((a&(!b))|(b^c))
[size 8] (((!a)&b)|(a^c))
[size 8] (((!a)&b)|(b^c))
[size 8] (a^(b|(!(a^c))))
[size 8] (a^(b^(a&(!c))))
[size 8] (a^(b^(c&(!a))))
[size 8] (a^(b^(a|(!c))))
[size 8] (a^(b^(c|(!a))))
[size 8] ((a|(b|(!c)))^b)
[size 8] (((!a)|(b|c))^b)
[size 9] (!((a|(!b))&(b^c)))
[size 9] (!(((!a)|b)&(b^c)))
[size 9] (!((a^b)|(a&(!c))))
[size 9] (!((a^b)|(c&(!a))))
[size 9] (!((a&(!b))|(a^c)))
[size 9] (!((a&(!b))|(b^c)))
[size 9] (!(((!a)&b)|(a^c)))
[size 9] (!(((!a)&b)|(b^c)))
[size 9] ((a|b)&(c|(a&b)))
[size 9] ((a|b)&(a^(b^c)))
[size 9] ((a|b)&(c^(a&b)))
[size 9] ((a^(b^c))&(a|c))
[size 9] ((a^(b^c))&(b|c))
[size 9] ((a&b)|(a^(b^c)))
[size 9] ((a^(b|c))|(b&c))
[size 9] ((a^(b^c))|(a&c))
[size 9] (a^((a^b)&(b^c)))
[size 9] (a^((b^c)&(c^a)))
[size 9] (a^((a&b)|(b^c)))
[size 9] (a^((a^b)|(a^c)))
""" 
# (Note: I truncated the list here for readability. 
# Drop your full list into this multiline string.)

# Run the search
matches = find_equivalent(target_expr, raw_expression_list)

# generate the truth table for the target expression and compare it against the truth tables of the expressions in the list.
print("Truth Table for "+target_expr+":")
print(get_truth_table(target_expr))
print("\nTruth Table for matches:")
for size, expr in matches:
    print(f"{expr} (size {size}): {get_truth_table(expr)}")
