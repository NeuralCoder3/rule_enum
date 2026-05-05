FROM ocaml/opam:alpine-3.20-ocaml-5.3

USER root
RUN apk add --no-cache gmp-dev z3-dev
USER opam

RUN opam install -y dune z3

WORKDIR /build
COPY --chown=opam . .

RUN opam exec -- dune build --profile=release \
 && cp _build/default/bin/main.exe /home/opam/rule_enum \
 && chmod +x /home/opam/rule_enum

USER root
RUN cp /home/opam/rule_enum /usr/local/bin/rule_enum
RUN ldd /usr/local/bin/rule_enum 2>&1 || echo "static binary?"

ENTRYPOINT ["/usr/local/bin/rule_enum"]
CMD ["--help"]
