#!/usr/bin/env bash

DATE=""

# this script requires a session cookie from adventofcode.com to be set in the SESSION env var

# get the current day of month from param or use today
if [ -z "$1" ]; then
    # when using date, remove 0 padding
    DATE=$(date +%d | sed 's/^0*//')
else
    DATE=$1
fi

# get the current year from param or use today
if [ -z "$2" ]; then
    YEAR=$(date +%Y)
else
    YEAR=$2
fi

PADDED_DATE=$(printf "%02d" "$DATE")

echo "Setting up day $DATE"

touch "./tests/day${PADDED_DATE}_1.txt"

curl "https://adventofcode.com/${YEAR}/day/${DATE}/input" -H "Cookie: session=$SESSION" >"inputs/day${PADDED_DATE}.txt"

# detect language used, because file structure is different
# check if the current path contains "rust" or "go"

if [[ $PWD == *"rust"* ]]; then
    echo "Rust project detected"

    # exit if target dir exists
    if [ -d "days/${PADDED_DATE}" ]; then
        echo "folder for $DATE already exists"
        exit 1
    fi

    cp -r "days/00" "days/${PADDED_DATE}"

    # replace the day number "day00" in Cargo.toml
    sed -i "s/day00/day${PADDED_DATE}/g" "days/${PADDED_DATE}/Cargo.toml"

    # and in main.rs change the vars
    sed -i "s/= 0;/= ${DATE};/g" "days/${PADDED_DATE}/src/main.rs"

    exit 0
fi

if [[ $PWD == *"go"* ]]; then
    echo "Go project detected"
    # go subproject does not have the days folder, folders are only the padded day number

    # exit if target dir exists
    if [ -d "${PADDED_DATE}" ]; then
        echo "folder for $DATE already exists"
        exit 1
    fi

    cp -r "00" "${PADDED_DATE}"

    # replace the day number "00" in the go files
    sed -i "s/00/${PADDED_DATE}/g" "${PADDED_DATE}"/*.go

    exit 0
fi
