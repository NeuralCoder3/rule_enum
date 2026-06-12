file = "output2/bool_vcs3_v4.rules"

placeholders = ["A", "B", "C"]

placeholder_count = 0
variable_count = 0

for line in open(file):
    line = line.strip()
    if not line:
        continue
    placeholder_rule = False
    for ph in placeholders:
        if ph in line:
            placeholder_rule = True
            break
    if placeholder_rule:
        placeholder_count += 1
    else:
        variable_count += 1
        
print(f"Placeholder rules: {placeholder_count}")
print(f"Variable rules: {variable_count}")
print(f"Total rules: {placeholder_count + variable_count}")