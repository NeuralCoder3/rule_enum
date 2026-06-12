import sys
from rules_to_ruler import ASTNode, PrattParser
import matplotlib.pyplot as plt

logic_symbols = [
    ("~", 1, 40), 
    ("&", 2, 30), 
    ("^", 2, 20),
    ("|", 2, 10),
]
parser = PrattParser(logic_symbols)

def size_of_term(term):
    return 1 + sum(size_of_term(arg) for arg in term.args)

if __name__ == "__main__":
    if len(sys.argv) != 4:
        print("Usage: python term_size_counter.py <input_file> <output_file> <plot_file>")
        sys.exit(1)
    with open(sys.argv[1], 'r') as f:
        data = f.readlines()
    
    counter_dict = {}
    for term in data:
        term = parser.parse(term.strip())
        size = size_of_term(term)
        counter_dict[size] = counter_dict.get(size, 0) + 1

    with open(sys.argv[2], 'w') as f:
        for size, count in sorted(counter_dict.items()):
            f.write(f"{size}: {count}\n")

    # Create a simple bar chart
    sizes = list(counter_dict.keys())
    counts = list(counter_dict.values())

    plt.bar(sizes, counts)
    plt.xlabel("Term Size")
    plt.ylabel("Frequency")
    plt.title("Distribution of Term Sizes")
    plt.savefig(sys.argv[3])