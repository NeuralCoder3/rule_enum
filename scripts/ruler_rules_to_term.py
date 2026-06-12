import json
import sys

class Term:
    def __init__(self, name, args):
        self.name = name
        self.args = args

    def __repr__(self):
        if len(self.args) == 2:
            return f"({self.args[0]} {self.name} {self.args[1]})"
        if self.args:
            return f"({self.name} {' '.join(map(str, self.args))})"
        else:
            return self.name

def parse_sexpr(expression: str):
    """Parses an S-expression string into Term objects."""
    
    tokens = expression.replace('(', ' ( ').replace(')', ' ) ').split()
    
    def process_tokens(tokens):
        if not tokens:
            raise SyntaxError("Unexpected EOF while parsing")
        
        token = tokens.pop(0)
        
        if token == '(':
            if not tokens or tokens[0] == ')':
                raise SyntaxError("Empty parentheses '()' cannot be parsed into a Term")
                
            name = tokens.pop(0)
            args = []
            
            while tokens and tokens[0] != ')':
                args.append(process_tokens(tokens))
            
            if not tokens:
                raise SyntaxError("Missing closing ')'")
            
            tokens.pop(0)  
            return Term(name, args)
            
        elif token == ')':
            raise SyntaxError("Unexpected ')'")
            
        else:
            try:
                return int(token)
            except ValueError:
                try:
                    return float(token)
                except ValueError:
                    return token

    return process_tokens(tokens)

# def parse_sexpr(sexpr):
#     if sexpr[0] == '(':
#         assert sexpr[-1] == ')', "Mismatched parentheses"
#         sexpr = sexpr[1:-1].strip()
#     parts = []
#     while sexpr:
#         if sexpr[0] == '(':
#             part, sexpr = parse_sexpr(sexpr)
#             parts.append(part)
#         else:
#             space_index = sexpr.find(' ')
#             if space_index == -1:
#                 parts.append(sexpr)
#                 sexpr = ''
#             else:
#                 parts.append(sexpr[:space_index])
#                 sexpr = sexpr[space_index + 1:].strip()
#     expr = Term(parts[0], parts[1:])
#     return expr, sexpr


if __name__ == "__main__":
    if len(sys.argv) != 2:
        print("Usage: python rules_to_terms.py <input_file>")
        sys.exit(1)
    with open(sys.argv[1], 'r') as f:
        data = json.load(f)
    
    for rule in data['eqs']:
        lhs = rule['lhs']
        rhs = rule['rhs']
        lhs_term = parse_sexpr(lhs)
        rhs_term = parse_sexpr(rhs)
        # print(f"{lhs_term} = {rhs_term}")
        print(lhs_term)
        print(rhs_term)