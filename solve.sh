#!/bin/sh

if [ $# -eq 0 ]; then
    echo "usage: solve.sh <problem_number>"
    exit 1
fi

sol=$(printf "day%02d" $1)

folder="./src/${sol}"
config="Release"
runtime="net10.0"
exe="${folder}/bin/${config}/${runtime}/${sol}"

if [ ! -x ${exe} ]; then
   dotnet build ${folder}/${sol}.fsproj -c Release --no-restore
fi

${exe} ${folder}/input 

echo
