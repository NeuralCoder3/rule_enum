# 2.1s
dune build --profile=release bin/main.exe && ./_build/default/bin/main.exe $@

# 2.1s
# dune exec --profile=release rule_enum

# (ocamlopt_flags (:standard -O3 -unbox-closures))

# ./run_opt.sh --domain bool --max-vars 2 --max-size 1000 --full --random-inputs 0 --output output/rules_bool_2_v12.txt --stats output/stats_bool_2_v12.csv

# ./run_opt.sh --domain bool --max-vars 3 --max-size 1000 --full --random-inputs 0 --output output/rules_bool_3_v13.txt --stats output/stats_bool_3_v13.csv --jobs 1