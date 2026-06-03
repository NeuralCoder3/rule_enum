# 2.1s
dune build --profile=release bin/main.exe && ./_build/default/bin/main.exe $@

# 2.1s
# dune exec --profile=release rule_enum

# (ocamlopt_flags (:standard -O3 -unbox-closures))

# ./run_opt.sh --domain bool --max-vars 2 --max-size 1000 --full --random-inputs 0 --output output/rules_bool_2_v12.txt --stats output/stats_bool_2_v12.csv
# ./run_opt.sh --domain bool --max-vars 3 --max-size 1000 --full --random-inputs 0 --output output/rules_bool_3_v13.txt --stats output/stats_bool_3_v13.csv --jobs 1



# export VERSION=7;export RULE_ENUM_BV_WIDTH=32;export RULE_ENUM_SMT_TIMEOUT_MS=200; ./run_opt.sh --domain bv --max-vcs 3 --max-size 100 --random-inputs 100 --smt --stats output2/bv_vcs3_v$VERSION.csv --output output2/bv_vcs3_v$VERSION.txt --rule-output output2/bv_vcs3_v$VERSION.rules --irred-output output2/bv_vcs3_v$VERSION.irs --jobs 4 | tee -a output2/bv_vcs3_v$VERSION.log
# rm output2/bv_*_v1.*
# --smt-unknown-inputs
# --safe-mode
# export VERSION=8;export RULE_ENUM_BV_WIDTH=4;./run_opt.sh --domain bv --max-vcs 3 --max-size 100 --random-inputs 100 --smt --stats output2/bv4_vcs3_v$VERSION.csv --output output2/bv4_vcs3_v$VERSION.txt --rule-output output2/bv4_vcs3_v$VERSION.rules --irred-output output2/bv4_vcs3_v$VERSION.irs --jobs 4 --progress | tee -a output2/bv4_vcs3_v$VERSION.log

# bool
# Final [1004.6s]: SR=255269  KR=699  IR=256, T20
# export VERSION=4;./run_opt.sh --domain bool --max-vcs 3 --max-size 100 --full --random-inputs 0 --stats output2/bool_vcs3_v$VERSION.csv --output output2/bool_vcs3_v$VERSION.txt --rule-output output2/bool_vcs3_v$VERSION.rules --irred-output output2/bool_vcs3_v$VERSION.irs --jobs 4  --progress | tee -a output2/bool_vcs3_v$VERSION.log




#   echo '(b+a)
#   (B*(A*B))
#   ((a+b)+c)' > input.txt
#   ./run_opt.sh --domain bv --eval --rules-input output2/bv4_vcs3_v8.rules --terms-input input.txt --output normalized.txt