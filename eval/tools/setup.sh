#!/usr/bin/env bash
# Builds the external tools of the evaluation in build/: Ruler and Twee cloned at the
# commits used, with the local additions of this directory, and the Python environment
# with egglog, matplotlib and z3 in venv/.
# Needs git, cargo, cmake, g++, cabal with GHC 9.12.2 ($GHC) and Python 3.14 ($PYTHON).
set -euo pipefail
cd "$(dirname "$0")"
mkdir -p build bin

clone() {  # URL COMMIT DIR
  [ -d "$3" ] || git clone -q "$1" "$3"
  git -C "$3" checkout -q --detach "$2"
}

# Ruler (OOPSLA'21) with our int domain; z3 is built from source, whose scripts import
# distutils (Python < 3.12).
clone https://github.com/uwplse/ruler.git e4217ed4b175ad1e7601790dd229fc1d857eb463 build/ruler
cp ruler/int.rs build/ruler/src/bin/
cp ruler/Cargo.lock build/ruler/
shim=$(mktemp -d)
trap 'rm -rf "$shim"' EXIT
if ! python3 -c 'import distutils' 2> /dev/null; then
  ln -s "$(command -v "${Z3_PYTHON:-python3.11}")" "$shim/python3"
fi
(cd build/ruler && PATH="$shim:$PATH" RUSTFLAGS="-C target-cpu=x86-64" CMAKE_POLICY_VERSION_MINIMUM=3.5 \
  CXXFLAGS=-Wno-template-body cargo build --release --locked --bin bool --bin int --bin bv4 --bin bv32)

# Twee 2.6 with the dependency versions of twee/cabal.project.freeze.
ghc=${GHC:-ghc-9.12.2}
clone https://github.com/nick8325/twee.git 9dc5b6105c2b21d32fd2a4c41b427aec915d3ca5 build/twee
cp twee/cabal.project.freeze build/twee/
(cd build/twee && cabal update && cabal build -w "$ghc" exe:twee)
cp "$(cd build/twee && cabal list-bin -w "$ghc" twee)" bin/twee

"${PYTHON:-python3.14}" -m venv venv
venv/bin/pip install -q --no-deps --require-hashes -r requirements.txt
venv/bin/pip check
