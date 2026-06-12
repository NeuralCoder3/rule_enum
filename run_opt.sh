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
# export VERSION=14;export RULE_ENUM_BV_WIDTH=4;./run_opt.sh --domain bv --max-vcs 3 --max-size 100 --random-inputs 100 --smt --stats output2/bv4_vcs3_v$VERSION.csv --output output2/bv4_vcs3_v$VERSION.txt --rule-output output2/bv4_vcs3_v$VERSION.rules --irred-output output2/bv4_vcs3_v$VERSION.irs --jobs 4 --progress | tee -a output2/bv4_vcs3_v$VERSION.log

# bool
# Final [1004.6s]: SR=255269  KR=699  IR=256, T20
# export VERSION=4;./run_opt.sh --domain bool --max-vcs 3 --max-size 100 --full --random-inputs 0 --stats output2/bool_vcs3_v$VERSION.csv --output output2/bool_vcs3_v$VERSION.txt --rule-output output2/bool_vcs3_v$VERSION.rules --irred-output output2/bool_vcs3_v$VERSION.irs --jobs 4  --progress | tee -a output2/bool_vcs3_v$VERSION.log




#   echo '(b+a)
#   (B*(A*B))
#   ((a+b)+c)' > input.txt
#   ./run_opt.sh --domain bv --eval --rules-input output2/bv4_vcs3_v8.rules --terms-input input.txt --output normalized.txt


# export VERSION=1;./run_opt.sh --domain demo --max-vcs 3 --max-size 100 --smt --random-inputs 200 --stats output2/demo_vcs3_v$VERSION.csv --output output2/demo_vcs3_v$VERSION.txt --rule-output output2/demo_vcs3_v$VERSION.rules --irred-output output2/demo_vcs3_v$VERSION.irs --jobs 4  --progress | tee -a output2/demo_vcs3_v$VERSION.log



# Eval:
# === bool, 3 vars
# ./run_opt.sh --domain bool --max-vcs 3 --max-size 100 --full --random-inputs 0 --stats eval/bool_vcs3.csv --output eval/bool_vcs3.txt --rule-output eval/bool_vcs3.rules --irred-output eval/bool_vcs3.irs --jobs 4  --progress | tee -a eval/bool_vcs3.log
#    currently until 14
# export SIZE=5;./run_opt.sh --domain bool --max-vcs 3 --max-size $SIZE --full --random-inputs 0 --stats eval/bool_vcs3.csv --output eval/bool_vcs3_s$SIZE.txt --rule-output eval/bool_vcs3_s$SIZE.rules --irred-output eval/bool_vcs3_s$SIZE.irs --jobs 4  --progress | tee -a eval/bool_vcs3_s$SIZE.log
# === no vars, but placeholders
# ./run_opt.sh --domain bool --max-vars 0 --max-holes 3 --max-size 100 --full --random-inputs 0 --stats eval/bool_v0_c3.csv --output eval/bool_v0_c3.txt --rule-output eval/bool_v0c3.rules --irred-output eval/bool_v0c3.irs --jobs 4  --progress | tee -a eval/bool_v0c3.log
#    all done
# max size n:
# export SIZE=5;./run_opt.sh --domain bool --max-vars 0 --max-holes 3 --max-size $SIZE --full --random-inputs 0 --stats eval/bool_v0_c3.csv --output eval/bool_v0_c3_s$SIZE.txt --rule-output eval/bool_v0c3_s$SIZE.rules --irred-output eval/bool_v0c3_s$SIZE.irs --jobs 4  --progress | tee -a eval/bool_v0c3_s$SIZE.log
#
#
# === bv4, 3 vars
# export RULE_ENUM_BV_WIDTH=4;./run_opt.sh --domain bv --max-vcs 3 --max-size 100 --random-inputs 200 --smt --stats eval/bv4_vcs3.csv --output eval/bv4_vcs3.txt --rule-output eval/bv4_vcs3.rules --irred-output eval/bv4_vcs3.irs --jobs 4 --progress | tee -a eval/bv4_vcs3.log
#    currently until 7
#
#
# === int, 3 vars
# ./run_opt.sh --domain int --max-vcs 3 --max-size 100 --random-inputs 200 --smt --stats eval/int_vcs3.csv --output eval/int_vcs3.txt --rule-output eval/int_vcs3.rules --irred-output eval/int_vcs3.irs --jobs 4 --progress | tee -a eval/int_vcs3.log
#    currently until 11
#
#
# === visualize
# python scripts/log2csv.py eval/*.log
# for file in eval/*.csv; do python scripts/visualize.py "$file" --no-show --log; done
# 
#
# === ruler rules (oopsla21-aec)
# CXXFLAGS="-Wno-template-body" cargo build
# CMAKE_POLICY_VERSION_MINIMUM=3.5 CXXFLAGS="-Wno-template-body" cargo build --release
# run ruler:
# ./target/debug/bool synth --variables 3 --iters 2 --rules-to-take 0 --outfile bool_3_2_0.json
# ./target/debug/bool synth --variables 3 --iters 2 --use-smt --rules-to-take 0 --outfile bool_3_2_0_smt.json
# 
# show our imply theirs
# ./run_opt.sh --domain bool --eval --rules-input eval/bool_v0c3.rules --terms-input ruler_bool_3_2_0.txt --output ruler_bool_3_2_0_norm_v0c3.txt
# ./run_opt.sh --domain bool --eval --rules-input eval/bool_vcs3.rules --terms-input ruler_bool_3_2_0.txt --output ruler_bool_3_2_0_norm_vcs3.txt
# ./run_opt.sh --domain bool --eval --rules-input eval/bool_v0c3.rules --terms-input ruler_bool_3_4_0.txt --output ruler_bool_3_4_0_norm_v0c3.txt
# 
# rulers imply ours (in ruler setting):
# python scripts/rules_to_ruler.py eval/bool_v0c3.rules eval/ruler/bool_v0c3.json
# python scripts/rules_to_ruler.py eval/bool_v0c3_s5.rules eval/ruler/bool_v0c3_s5.json
# python scripts/rules_to_ruler.py eval/bool_v0c3_s7.rules eval/ruler/bool_v0c3_s7.json
# python scripts/rules_to_ruler.py eval/bool_v0c3_s9.rules eval/ruler/bool_v0c3_s9.json
# it2: max size 5
# it4: max size 9
# ./target/debug/bool derive ruler_bool_3_2_0.json bool_v0c3_s5.json derive_ruler_it2-v0c3_s5.json
#   all derived (18 <-> 154)
# ./target/debug/bool derive ruler_bool_3_2_0.json bool_v0c3_s7.json derive_ruler_it2-v0c3_s7.json  | tee derive_ruler_it2-v0c3_s7.log
#   ruler -> ours: 1310 derivable, 123 are not
# ./target/debug/bool derive ruler_bool_3_4_0.json bool_v0c3_s7.json derive_ruler_it4-v0c3_s7.json  | tee derive_ruler_it4-v0c3_s7.log
#   ruler -> ours: 1431 derivable, 2 are not
#   ours -> ruler: 32 derivable, 1 are not
# ./target/debug/bool derive ruler_bool_3_4_0.json bool_v0c3_s9.json derive_ruler_it4-v0c3_s9.json  | tee derive_ruler_it4-v0c3_s9.log
#   ruler -> ours: 8890 derivable, 645 are not
#   ours -> ruler: 33 derivable, 0 are not
#   
# 
#
# === random terms (with discrimination tree, out rules)
# python scripts/termgen/termgen.py -n 500 -k 3 --builtin bool --notation prefix --sample 1000 --seed 42 > scripts/termgen/bool_500_3.txt
# python scripts/termgen/termgen.py -n 50 -k 3 --builtin bool --notation prefix --sample 1000 --seed 42 > scripts/termgen/bool_50_3.txt
# python scripts/termgen/termgen.py -n 500 -k 3 --builtin bool --notation sexpr --sample 1000 --seed 42 > eval/terms/bool_500_3_sexpr.txt
# python scripts/termgen/termgen.py -n 50 -k 3 --builtin bool --notation sexpr --sample 1000 --seed 42 > eval/terms/bool_50_3_sexpr.txt
# ./run_opt.sh --domain bool --eval --rules-input eval/bool_vcs3.rules --terms-input scripts/termgen/bool_50_3.txt --output eval/terms/bool_50_3_norm_vcs3.txt
# general, size 5, size 9
# vars vcs
# 50, 500
# listen for ctrl+c
# for suffix in "" "_s5" "_s9"; do
#     for rules in "bool_v0c3${suffix}" "bool_vcs3${suffix}"; do
#         for size in 50 500; do
#             echo -e "\n\n=== Evaluating rules ${rules} on size ${size} ==="
#             ./run_opt.sh --domain bool --eval \
#                 --rules-input "eval/${rules}.rules" \
#                 --terms-input "scripts/termgen/bool_${size}_3.txt" \
#                 --output "eval/terms/norm_term_${size}_${rules}.txt" < /dev/null
#         done
#     done
# done
#
# find all files call python scripts/term_size_counter.py eval/terms/norm_term_50_bool_v0c3_s5.txt eval/terms/count_norm_term_50_bool_v0c3_s5.txt eval/terms/count_norm_term_50_bool_v0c3_s5.png
# for file in eval/terms/norm_*.txt; do python scripts/term_size_counter.py "$file" "${file%.txt}.count" "${file%.txt}.png"; done
#
#
#
# === random terms (with egraph, ruler rules)
# scripts/egglog/venv/bin/python scripts/egglog/simplify.py \
#     eval/ruler/ruler_bool_3_2_0.json \
#     eval/terms/bool_50_3.txt \
#     eval/terms/eqsat_ruler_it2_bool_50_3__it2_parallel.txt \
#     --mode parallel --iters 2 \
#     --in-notation prefix --out-notation infix
# TODO: more
#
# === random terms (with egraph, our rules)
# scripts/egglog/venv/bin/python scripts/egglog/simplify.py \
#     eval/ruler/bool_v0c3_s5.json \
#     eval/terms/bool_50_3.txt \
#     eval/terms/eqsat_v0c3_s5__bool_50_3__it2_parallel.txt \
#     --mode parallel --iters 2 \
#     --in-notation prefix --out-notation infix
# TODO: more
#
#
# === random terms (greedy, ruler rules) 
# python3 scripts/ruler_to_rules.py eval/ruler/ruler_bool_3_2_0.json
# ./run_opt.sh --domain bool --eval --rules-input eval/ruler/ruler_bool_3_2_0.rules --terms-input "eval/terms/bool_50_3.txt" --output "eval/terms/ruler_term_50__3_2_0.txt" < /dev/null
# ./run_opt.sh --domain bool --eval --rules-input eval/ruler/ruler_bool_3_2_0.rules --terms-input "scripts/termgen/bool_${size}_3.txt" --output "eval/terms/norm_term_${size}_${rules}.txt" < /dev/null
# 
# ruler rules greedy
# TODO: other domains
#
#     \item run int, bv4, bool as far as they go
#     \item prove rulers rules with ours
#     \item prove our rules in ruler
#     \item simplify random terms with our rules
#         \begin{itemize}
#             \item greedy
#             \item discrimination tree
#             \item egraph
#         \end{itemize}
#     \item with variables vs holes only: system comparison
#     \item with variables vs holes only: optimization potential (optional)
# 
# egglog
# 