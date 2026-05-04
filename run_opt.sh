# 2.1s
dune build --profile=release bin/main.exe && ./_build/default/bin/main.exe

# 2.1s
# dune exec --profile=release rule_enum

# (ocamlopt_flags (:standard -O3 -unbox-closures))