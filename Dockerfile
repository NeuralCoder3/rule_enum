FROM ocaml/opam:alpine-3.20-ocaml-5.3

USER root
# 1. Install system dependencies so opam can detect and use the system Z3 library
RUN apk add --no-cache gmp-dev z3-dev z3 pkgconf python3
USER opam

# 2. Install dune and z3 (removed conf-z3). 
# The z3 package will use pkgconf to find the Alpine system library automatically.
RUN opam install -y dune z3

WORKDIR /build
COPY --chown=opam . .
RUN opam exec -- dune build --profile=release \
 && cp _build/default/bin/main.exe /home/opam/rule_enum \
 && chmod +x /home/opam/rule_enum

USER root
RUN cp /home/opam/rule_enum /usr/local/bin/rule_enum

# 3. Failsafe: if opam still dynamically built its own libz3.so, move it to the system lib path
RUN find /home/opam/.opam -name "libz3.so" -exec cp {} /usr/lib/ \; || true

# 4. Check dynamic linkages
RUN ldd /usr/local/bin/rule_enum 2>&1 || echo "static binary?"

ENTRYPOINT ["/usr/local/bin/rule_enum"]
CMD ["--help"]