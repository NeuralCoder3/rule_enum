import itertools

# ==========================================
# 1. EXPLICIT VARIABLE SETS
# ==========================================
# You can leave either set empty, e.g., VAR_SET_2 = []
VAR_SET_1 = ['a', 'b', 'c']
# VAR_SET_2 = ['A', 'B', 'C']
VAR_SET_2 = []

# ==========================================
# 2. CONFIGURATION TOGGLES
# ==========================================
# True  = Deduplicate renamings (variables are symmetric/interchangeable)
# False = Do NOT deduplicate (variables are rigid/distinct)
DEDUP_SET_1 = True
DEDUP_SET_2 = False

# ==========================================

ALL_VARS = VAR_SET_1 + VAR_SET_2
NUM_VARS = len(ALL_VARS)
TT_SIZE = 1 << NUM_VARS  # Truth table size (e.g., 64 for 6 vars, 8 for 3 vars)

def generate_valid_functions():
    """
    Generates all distinct truth tables that depend on at most 3 
    out of the available variables.
    """
    valid_functions = set()
    max_vars = min(4, NUM_VARS + 1)
    
    for r in range(max_vars):
        for subset in itertools.combinations(range(NUM_VARS), r):
            num_functions = 1 << (1 << r)
            for f in range(num_functions):
                tt = 0
                for k in range(TT_SIZE):
                    idx = 0
                    for bit_pos, var_idx in enumerate(subset):
                        if k & (1 << var_idx):
                            idx |= (1 << bit_pos)
                    if f & (1 << idx):
                        tt |= (1 << k)
                valid_functions.add(tt)
                
    return valid_functions

def get_essential_vars(tt):
    """
    Determines which variables a truth table actually depends on.
    """
    essential = []
    for i in range(NUM_VARS):
        depends = False
        for k in range(TT_SIZE):
            val1 = bool(tt & (1 << k))
            val2 = bool(tt & (1 << (k ^ (1 << i))))
            if val1 != val2:
                depends = True
                break
        if depends:
            essential.append(ALL_VARS[i])
    return essential

def get_permutations(indices, dedup):
    """Helper to safely handle permutations even if a set is empty."""
    if not indices:
        return [()]
    if dedup:
        return list(itertools.permutations(indices))
    else:
        return [tuple(indices)] # Identity permutation only

def find_equivalence_classes(valid_functions):
    """
    Groups functions into equivalence classes dynamically based on toggles.
    """
    # Assign indices based on the sizes of the sets
    idx_1 = list(range(len(VAR_SET_1)))
    idx_2 = list(range(len(VAR_SET_1), NUM_VARS))
    
    perms_1 = get_permutations(idx_1, DEDUP_SET_1)
    perms_2 = get_permutations(idx_2, DEDUP_SET_2)
        
    group = [p1 + p2 for p1 in perms_1 for p2 in perms_2]
            
    # Precompute bit-mappings
    k_mapped = {}
    for pi in group:
        mapping = []
        for k in range(TT_SIZE):
            new_k = 0
            for i in range(NUM_VARS):
                if k & (1 << i):
                    new_k |= (1 << pi[i])
            mapping.append(1 << new_k)
        k_mapped[pi] = mapping

    visited = set()
    classes = []
    
    for tt in sorted(valid_functions):
        if tt not in visited:
            orbit = set()
            for pi in group:
                mapping = k_mapped[pi]
                new_tt = 0
                temp = tt
                k = 0
                while temp:
                    if temp & 1:
                        new_tt |= mapping[k]
                    temp >>= 1
                    k += 1
                orbit.add(new_tt)
                visited.add(new_tt)
            classes.append(orbit)
            
    return classes

def print_classes(classes):
    """Prints the equivalence classes beautifully."""
    hex_width = max(2, (TT_SIZE + 3) // 4) # Adjust hex padding dynamically
    
    print("-" * 80)
    print(f"{'Class':<8} | {'Size':<6} | {'Canonical Truth Table':<{hex_width + 4}} | {'Essential Variables'}")
    print("-" * 80)
    
    for i, orbit in enumerate(classes, 1):
        canonical_tt = min(orbit)
        orbit_size = len(orbit)
        ess_vars = get_essential_vars(canonical_tt)
        
        hex_str = f"0x{canonical_tt:0{hex_width}X}"
        vars_str = f"[{', '.join(ess_vars)}]" if ess_vars else "[None (Constant)]"
        
        print(f"{i:<8} | {orbit_size:<6} | {hex_str:<{hex_width + 4}} | {vars_str}")
    print("-" * 80)

if __name__ == "__main__":
    print(f"Total Variables: {NUM_VARS} {ALL_VARS}")
    print(f"Set 1 ({VAR_SET_1}) Deduplication: {'ON' if DEDUP_SET_1 else 'OFF'}")
    print(f"Set 2 ({VAR_SET_2}) Deduplication: {'ON' if DEDUP_SET_2 else 'OFF'}")
    
    print(f"\nGenerating distinct functions (max 3 of {NUM_VARS} variables)...")
    functions = generate_valid_functions()
    print(f"Generated {len(functions)} valid distinct functions.")
    
    print("\nCalculating equivalence classes...")
    classes = find_equivalence_classes(functions)
    
    print(f"\nTotal Equivalence Classes found: {len(classes)}\n")
    print_classes(classes)