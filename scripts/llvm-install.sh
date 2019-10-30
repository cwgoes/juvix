#!/usr/bin/env bash
set -eou pipefail

MIRROR="http://releases.llvm.org"

# Download
echo "Downloading: llvm-${LLVM_VERSION}.src.tar.xz"
wget -nv -O "llvm-${LLVM_VERSION}.src.tar.xz"     "${MIRROR}/${LLVM_VERSION}/llvm-${LLVM_VERSION}.src.tar.xz"

# Extract
echo "Extracting: llvm-${LLVM_VERSION}.src.tar.xz"
tar xf "llvm-${LLVM_VERSION}.src.tar.xz"

cd ./llvm-${LLVM_VERSION}.src

mkdir build
cd build

# Building
echo "Building: llvm-${LLVM_VERSION}.src.tar.xz"
cmake -DLLVM_INCLUDE_TESTS=false -DLLVM_BUILD_LLVM_DYLIB=true -DLLVM_LINK_LLVM_DYLIB=true ../

cmake --build .

# Building
echo "Installing llvm-${LLVM_VERSION}.src.tar.xz"
cmake --build . --target install

cd ../..

# Cleanup
rm llvm-${LLVM_VERSION}.src.tar.xz
rm -rf llvm-${LLVM_VERSION}.src
