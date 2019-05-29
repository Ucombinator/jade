#!/usr/bin/env bash

# See https://stackoverflow.com/a/246128 or https://stackoverflow.com/questions/59895/get-the-source-directory-of-a-bash-script-from-within-the-script-itself/246128#246128
: ${source:="${BASH_SOURCE[0]}"}
while [ -h "$source" ]; do # resolve $JADE_source until the file is no longer a symlink
dir="$( cd -P "$( dirname "$source" )" >/dev/null 2>&1 && pwd )"
source="$(readlink "$source")"
[[ $source != /* ]] && source="$dir/$source" # if $JADE_source was a relative symlink, we need to resolve it relative to the path where the symlink file was located
done
dir="$( cd -P "$( dirname "$source" )/.." >/dev/null 2>&1 && pwd )"

set -o errexit
shopt -s nullglob

cd "$dir"

for i in $(seq 8 12); do
  echo "Java Language Specification $i"
  mkdir -p specifications/jls-$i
  for j in $(seq 1 19); do
    file=specifications/jls-$i/jls-$i-chapter-$j.html
    echo -n "  Downloading chapter $j ..."
    if [[ -e $file ]]; then
      echo " already exists"
    else
      jade download-jls $i $j >$file
      echo " downloaded"
    fi
  done

  echo "Java Virtual Machine Specification $i"
  mkdir -p specifications/jvms-$i
  for j in $(seq 1 7); do
    file=specifications/jvms-$i/jvms-$i-chapter-$j.html
    echo -n "  Downloading chapter $j ..."
    if [[ -e $file ]]; then
      echo " already exists"
    else
      jade download-jvms $i $j >$file
      echo " downloaded"
    fi
  done
done
echo "Generating code for Java $i grammar" # `$i` will be the last value of the previous loop
jade generate-modifier-code <specifications/jvms-$i/jvms-$i-chapter-4.html >src/main/scala/org/ucombinator/jade/classfile/Modifier.scala
echo "Generating code for ASM instruction types"
jade generate-asm-instruction-types >src/main/scala/org/ucombinator/jade/asm/InstructionTypes.scala
