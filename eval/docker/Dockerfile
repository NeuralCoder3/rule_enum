# syntax=docker/dockerfile:1
#   docker build -f eval/docker/Dockerfile -t rule-enum-eval .

ARG DEBIAN_SNAPSHOT=20261001T000000Z
ARG RUST_IMAGE=rust:1.88.0-slim-bookworm@sha256:38bc5a86d998772d4aec2348656ed21438d20fcdce2795b56ca434cf21430d89
ARG HASKELL_IMAGE=haskell:9.12.2-slim-bookworm@sha256:c83dcb092fff03ab85a6febf903f7ac378983ddadda53bb6da31ea74d70546f4
ARG OCAML_IMAGE=ocaml/opam:debian-12-ocaml-5.2@sha256:b06c7348b6ed3e83b5f66267254504a8f066ec9be9c020c30a8e28ce10ab3ef6
ARG PYTHON_IMAGE=python:3.14.7-slim-bookworm@sha256:82bc3c539b8813ada9d68c63b40158fa002f7f33de9bf3312a3dfdc0620dff56

# Ruler (OOPSLA'21) at commit e4217ed4, plus our int domain and Cargo.lock (eval/tools/ruler).
FROM ${RUST_IMAGE} AS ruler
ARG DEBIAN_SNAPSHOT
COPY eval/docker/apt-snapshot.sh /usr/local/bin/
RUN apt-snapshot.sh "$DEBIAN_SNAPSHOT" git ca-certificates cmake make g++ python3 python3-distutils
RUN git clone https://github.com/uwplse/ruler.git /ruler \
 && git -C /ruler checkout --detach e4217ed4b175ad1e7601790dd229fc1d857eb463
COPY eval/tools/ruler/Cargo.lock /ruler/
COPY eval/tools/ruler/int.rs /ruler/src/bin/
WORKDIR /ruler
# Portable code; z3 is built from source and linked statically.
ENV RUSTFLAGS="-C target-cpu=x86-64" CMAKE_POLICY_VERSION_MINIMUM=3.5 CXXFLAGS=-Wno-template-body
RUN cargo build --release --locked --bin bool --bin int --bin bv4 --bin bv32

# Twee 2.6 (commit 9dc5b610) with the dependency versions of eval/tools/twee/cabal.project.freeze.
FROM ${HASKELL_IMAGE} AS twee
ARG DEBIAN_SNAPSHOT
COPY eval/docker/apt-snapshot.sh /usr/local/bin/
RUN apt-snapshot.sh "$DEBIAN_SNAPSHOT" git ca-certificates libgmp-dev zlib1g-dev
RUN git clone https://github.com/nick8325/twee.git /twee \
 && git -C /twee checkout --detach 9dc5b6105c2b21d32fd2a4c41b427aec915d3ca5
COPY eval/tools/twee/cabal.project.freeze /twee/
WORKDIR /twee
RUN cabal update && cabal build exe:twee && cp "$(cabal list-bin twee)" /usr/local/bin/twee

# Our implementation; the test suite runs as part of the build.
FROM ${OCAML_IMAGE} AS ocaml
RUN opam install -y dune.3.20.2
WORKDIR /home/opam/rule_enum
COPY --chown=opam dune-project ./
COPY --chown=opam lib lib
COPY --chown=opam bin bin
COPY --chown=opam test test
RUN opam exec -- dune build --release @default @runtest

FROM ${PYTHON_IMAGE}
ARG DEBIAN_SNAPSHOT
COPY eval/docker/apt-snapshot.sh /usr/local/bin/
RUN apt-snapshot.sh "$DEBIAN_SNAPSHOT" libgmp10 libstdc++6
COPY eval/tools/requirements.txt /tmp/
RUN pip install --no-cache-dir --no-deps --require-hashes -r /tmp/requirements.txt && pip check
COPY --from=ruler /ruler/target/release/bool /ruler/target/release/int \
     /ruler/target/release/bv4 /ruler/target/release/bv32 /opt/ruler/
COPY --from=twee /usr/local/bin/twee /usr/local/bin/
COPY --from=ocaml /home/opam/rule_enum/_build/default/bin/main.exe /usr/local/bin/rule_enum
COPY --from=ocaml /home/opam/rule_enum/_build/default/test/test_rule_enum.exe /usr/local/bin/rule_enum_test
COPY eval/*.py eval/run.sh eval/bool_complete_generator.p /eval/
ENV BIN=/usr/local/bin/rule_enum TESTS=/usr/local/bin/rule_enum_test RULER=/opt/ruler EGGLOG_PY=python3 TWEE=/usr/local/bin/twee \
    OUT=/eval/out PYTHONHASHSEED=0 MPLCONFIGDIR=/tmp
WORKDIR /eval
ENTRYPOINT ["./run.sh"]
CMD ["all"]
