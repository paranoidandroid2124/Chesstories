#!/usr/bin/env bash
set -euo pipefail

readonly STOCKFISH_COMMIT='cb3d4ee9b47d0c5aae855b12379378ea1439675c'
readonly STOCKFISH_JS_COMMIT='31a98753a5d932511693f44775da908377c24513'
readonly EMSCRIPTEN_VERSION='3.1.7'
readonly NETWORK='nn-4ca89e4b3abf.nnue'
readonly NETWORK_SHA256='4ca89e4b3abfbe9df13e4f3db2acb64dc6ddc7a9becb2ac1cf388f4d66b3bd94'
readonly BASENAME='sf_18_smallnet_single'

script_dir=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
output_dir=${1:-"${script_dir}/assets"}

for command_name in emcc git make node sha256sum; do
  command -v "${command_name}" >/dev/null || {
    printf 'Missing build command: %s\n' "${command_name}" >&2
    exit 1
  }
done

actual_emscripten=$(emcc -dumpversion)
if [[ "${actual_emscripten}" != "${EMSCRIPTEN_VERSION}" ]]; then
  printf 'Emscripten %s is required; found %s.\n' "${EMSCRIPTEN_VERSION}" "${actual_emscripten}" >&2
  exit 1
fi

build_root=$(mktemp -d "${TMPDIR:-/tmp}/chesstories-move-review-t1.XXXXXXXX")
trap 'rm -rf -- "${build_root}"' EXIT
stockfish_dir="${build_root}/stockfish"
port_dir="${build_root}/stockfish-js"

git init -q "${stockfish_dir}"
git -C "${stockfish_dir}" fetch -q --depth=1 https://github.com/official-stockfish/Stockfish.git "${STOCKFISH_COMMIT}"
git -C "${stockfish_dir}" checkout -q --detach FETCH_HEAD
git -C "${stockfish_dir}" apply --whitespace=nowarn "${script_dir}/sf18-smallnet-single.patch"

git init -q "${port_dir}"
git -C "${port_dir}" fetch -q --depth=1 https://github.com/nmrugg/stockfish.js.git "${STOCKFISH_JS_COMMIT}"
git -C "${port_dir}" checkout -q --detach FETCH_HEAD
git -C "${port_dir}" rm -rq src
cp -a "${stockfish_dir}/src" "${port_dir}/src"
cp "${stockfish_dir}/scripts/net.sh" "${port_dir}/scripts/net.sh"

make -C "${port_dir}/src" net
printf '%s  %s\n' "${NETWORK_SHA256}" "${port_dir}/src/${NETWORK}" | sha256sum --check --strict

(
  cd "${port_dir}"
  node ./build.js \
    --single-threaded \
    --no-minify \
    --do-not-verify-nets \
    --basename "${BASENAME}" \
    --no-split \
    --force
)

mkdir -p "${output_dir}"
install -m 0644 "${port_dir}/src/${BASENAME}.js" "${output_dir}/${BASENAME}.js"
install -m 0644 "${port_dir}/src/${BASENAME}.wasm" "${output_dir}/${BASENAME}.wasm"
node "${script_dir}/verify.mjs" "${output_dir}/${BASENAME}.js"
sha256sum "${output_dir}/${BASENAME}.js" "${output_dir}/${BASENAME}.wasm"
