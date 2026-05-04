FROM ocaml/opam:alpine-3.20-ocaml-5.3 AS build

RUN opam install -y dune

WORKDIR /build
COPY --chown=opam . .

RUN opam exec -- dune build --profile=release \
 && cp _build/default/bin/main.exe /usr/local/bin/rule_enum

FROM alpine:3.20
COPY --from=build /usr/local/bin/rule_enum /usr/local/bin/rule_enum

ENTRYPOINT ["/usr/local/bin/rule_enum"]
CMD ["--help"]
