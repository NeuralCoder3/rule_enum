import math

def count_terms(n: int, c0: int, c1: int, c2: int) -> int:
    """
    Counts the number of possible terms of size n using Dynamic Programming.
    """
    if n <= 0:
        return 0
    if c0 == 0:
        return 0 # Without nullary symbols, no valid terms can be formed.

    # dp[i] will store the number of valid terms of size i
    dp = [0] * (n + 1)
    dp[1] = c0

    for i in range(2, n + 1):
        # 1. Unary contribution: c1 * T(i-1)
        unary_count = c1 * dp[i - 1]

        # 2. Binary contribution: c2 * sum(T(k) * T(i-1-k))
        binary_count = 0
        for k in range(1, i - 1):
            binary_count += dp[k] * dp[i - 1 - k]
        binary_count *= c2

        dp[i] = unary_count + binary_count

    return dp[n]


def enumerate_terms(n: int, sym0: list, sym1: list, sym2: list) -> list:
    """
    Explicitly builds all valid terms of size n as strings.
    """
    if n <= 0:
        return []
    if n == 1:
        return list(sym0) # Base case: return the nullary symbols

    terms = []

    # 1. Build Unary terms
    if sym1:
        # Get all sub-terms of size n-1
        sub_terms = enumerate_terms(n - 1, sym0, sym1, sym2)
        for f in sym1:
            for t in sub_terms:
                terms.append(f"{f}({t})")

    # 2. Build Binary terms
    if sym2:
        # Try all possible ways to split the remaining n-1 size between left and right arguments
        for k in range(1, n - 1):
            left_terms = enumerate_terms(k, sym0, sym1, sym2)
            right_terms = enumerate_terms(n - 1 - k, sym0, sym1, sym2)

            for h in sym2:
                for t1 in left_terms:
                    for t2 in right_terms:
                        terms.append(f"{h}({t1}, {t2})")

    return terms


def validate_logic(n: int, c0: int, c1: int, c2: int):
    """
    Validates that the mathematical count matches the explicit enumeration.
    """
    print(f"--- Validation for size n={n} | Nullary: {c0}, Unary: {c1}, Binary: {c2} ---")
    
    # Create dummy symbols for the enumerator to use
    sym0 = [f"v{i}" for i in range(1, c0 + 1)] # e.g., v1, v2
    sym1 = [f"f{i}" for i in range(1, c1 + 1)] # e.g., f1, f2
    sym2 = [f"g{i}" for i in range(1, c2 + 1)] # e.g., g1, g2

    # 1. Count via DP
    computed_count = count_terms(n, c0, c1, c2)
    
    # 2. Count via Enumeration
    enumerated = enumerate_terms(n, sym0, sym1, sym2)
    enumerated_count = len(enumerated)

    print(f"DP Computed Count:  {computed_count}")
    print(f"Enumerated Count:   {enumerated_count}")
    
    assert computed_count == enumerated_count, "Validation Failed: Counts do not match!"
    
    # Display the terms if the list is reasonably small
    if enumerated_count <= 25:
        print("Explicit Terms:")
        for term in enumerated:
            print(f"  {term}")
    elif enumerated_count > 0:
        print(f"Explicit Terms (Showing first 3 and last 3 of {enumerated_count}):")
        for term in enumerated[:3]:
            print(f"  {term}")
        print("  ...")
        for term in enumerated[-3:]:
            print(f"  {term}")
            
    print("✓ Validation passed.\n")

def estimate(n, c0,c1,c2):
    c = math.sqrt(c1+2*math.sqrt(c0*c2))*(c0*c2)**(0.25)/(2*c2*math.sqrt(math.pi))
    return c*(c1+2*math.sqrt(c0*c2))**(n)/(n**(1.5))
    # (sqrt(b+sqrt(a*c))*(a*c)**0.25)/(2*c*sqrt(pi))*((b+2*sqrt(a*c))**n)/(n**1.5)

def estimate_str(c0,c1,c2):
    c = math.sqrt(c1+2*math.sqrt(c0*c2))*(c0*c2)**(0.25)/(2*c2*math.sqrt(math.pi))
    a = c1+2*math.sqrt(c0*c2)
    # return c*a**(n)/(n**(1.5))
    print(f"c: {c:.2}, a: {a:.2}")
    print(f"Estimate: {c:.2} * {a:.2f}^n / n^1.5")

# --- Run Tests ---
if __name__ == "__main__":
    for n in [1,2,3,4,5,10,15,20]:
        bool_count = count_terms(n, c0=3, c1=1, c2=3)
        bv_count = count_terms(n, c0=3, c1=2, c2=7)
        # print scientific notation for large numbers
        print(f"Size n={n}:")
        print(f"  Bool: {bool_count:.2e}, {bool_count}")
        print(f"  BV: {bv_count:.2e}, {bv_count}")
        bool_est = estimate(n, c0=3, c1=1, c2=3)
        bv_est = estimate(n, c0=3, c1=2, c2=7)
        print(f"  Bool Estimate: {bool_est:.2e}")
        print(f"  BV Estimate: {bv_est:.2e}")
        # bool_est2=g
        
    print("bool")
    estimate_str(c0=3, c1=1, c2=3)
    print("bv")
    estimate_str(c0=3, c1=2, c2=7)

    # # Test 1: Simple case with 1 of each symbol, size 3
    # validate_logic(n=3, c0=1, c1=1, c2=1)

    # # Test 2: Heavier on nullary and binary symbols, size 4
    # validate_logic(n=4, c0=2, c1=0, c2=1)
    
    # # Test 3: Larger size to show the bounds (will output a truncated list)
    # validate_logic(n=5, c0=2, c1=2, c2=1)