import re
import json
import sys

class ASTNode:
    """Represents a node in the Abstract Syntax Tree."""
    def __init__(self, symbol, *args):
        self.symbol = symbol
        self.args = args

    def to_sexpr(self):
        """Converts the AST recursively into an S-expression string."""
        if not self.args:
            # Base case: Variable or Constant. 
            # If lowercase (a, b, c), append 'v'. If uppercase (A, B, C), just lowercase.
            if self.symbol.islower():
                return f"?{self.symbol}v"
            else:
                return f"?{self.symbol.lower()}"
        
        # Recursive case: Operator applied to arguments
        args_str = " ".join(arg.to_sexpr() for arg in self.args)
        return f"({self.symbol} {args_str})"

class PrattParser:
    def __init__(self, symbols):
        """
        symbols: list of tuples (symbol_string, arity, precedence)
        """
        self.prefix_ops = {}
        self.infix_ops = {}
        ops = set()
        
        for sym, arity, prec in symbols:
            ops.add(sym)
            if arity == 1:
                self.prefix_ops[sym] = prec
            elif arity == 2:
                self.infix_ops[sym] = prec
                
        # Sort operators by length so multi-char ops match first
        sorted_ops = sorted(ops, key=len, reverse=True)
        escaped_ops = [re.escape(op) for op in sorted_ops]
        
        # Regex matches operators, parentheses, and identifiers
        regex_pattern = '(' + '|'.join(escaped_ops) + r'|\(|\)|[A-Za-z0-9_]+)'
        self.tokenizer = re.compile(regex_pattern)

    def tokenize(self, expression):
        return [match.group(0).strip() for match in self.tokenizer.finditer(expression) if match.group(0).strip()]

    def parse(self, expression):
        self.tokens = self.tokenize(expression)
        self.pos = 0
        return self.parse_expr(0)

    def peek(self):
        return self.tokens[self.pos] if self.pos < len(self.tokens) else None

    def consume(self):
        token = self.peek()
        self.pos += 1
        return token

    def parse_expr(self, rbp):
        token = self.consume()
        if token is None:
            raise SyntaxError("Unexpected end of expression")
        
        # PREFIX
        if token == '(':
            left = self.parse_expr(0)
            if self.consume() != ')':
                raise SyntaxError("Expected closing parenthesis ')'")
        elif token in self.prefix_ops:
            prec = self.prefix_ops[token]
            operand = self.parse_expr(prec)
            left = ASTNode(token, operand)
        elif re.match(r'^[A-Za-z0-9_]+$', token):
            left = ASTNode(token)
        else:
            raise SyntaxError(f"Unexpected token: {token}")

        # INFIX
        while True:
            next_token = self.peek()
            if next_token not in self.infix_ops:
                break
            prec = self.infix_ops[next_token]
            if prec <= rbp:
                break
            
            self.consume()
            right = self.parse_expr(prec) 
            left = ASTNode(next_token, left, right)

        return left

def process_rules(input_filepath, output_filepath):
    # Standard logic operators with precedence
    logic_symbols = [
        ("~", 1, 40), 
        ("&", 2, 30), 
        ("^", 2, 20),
        ("|", 2, 10),
    ]
    parser = PrattParser(logic_symbols)
    
    eqs = []
    
    with open(input_filepath, 'r') as f:
        for line_num, line in enumerate(f, 1):
            line = line.strip()
            if not line:
                continue
                
            try:
                # Split based on arrows, but ignore the arrow type for the final JSON
                if "<->" in line:
                    left_str, right_str = line.split("<->")
                elif "->" in line:
                    left_str, right_str = line.split("->")
                else:
                    print(f"Skipping line {line_num}: No valid arrow found.")
                    continue
                    
                lhs_ast = parser.parse(left_str)
                rhs_ast = parser.parse(right_str)
                
                eqs.append({
                    "lhs": lhs_ast.to_sexpr(),
                    "rhs": rhs_ast.to_sexpr(),
                    "bidirectional": False  # Hardcoded per your request
                })
                
            except Exception as e:
                print(f"Error parsing line {line_num} ('{line}'): {e}")
                
    # Construct final JSON payload
    output_data = {
        "params": {},
        "time": 0,
        "num_rules": len(eqs),
        "smt_unknown": 0,
        "eqs": eqs
    }
    
    with open(output_filepath, 'w') as f:
        json.dump(output_data, f, indent=2)
        
    print(f"Successfully processed {len(eqs)} rules into {output_filepath}.")

# ==========================================
# Example Usage
# ==========================================
# if __name__ == "__main__":
#     test_input = "rules.txt"
#     test_output = "rules.json"
    
#     with open(test_input, "w") as f:
#         f.write("((~(A^((A^B)|(A^C))))&(~(A^((a^b)|(a^c))))) -> (~(A^((A^B)|(a^C))))\n")
#         f.write("(A & b) <-> (B & a)\n")
        
#     process_rules(test_input, test_output)
    
#     with open(test_output, "r") as f:
#         print(f.read())

if __name__ == "__main__":
    if len(sys.argv) != 3:
        print("Usage: python rules_to_ruler.py <input_file> <output_file>")
        sys.exit(1)
    input_file = sys.argv[1]
    output_file = sys.argv[2]
    process_rules(input_file, output_file)