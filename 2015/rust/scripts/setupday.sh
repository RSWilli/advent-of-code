#!/bin/bash

DATE=""
YEAR=2015

# get the vars from .env
# shellcheck source=/dev/null
source .env

# get the current day of month from param or use today
if [ -z "$1" ]; then
    # when using date, remove 0 padding
    DATE=$(date +%d | sed 's/^0*//')
else
    DATE=$1
fi

PADDED_DATE=$(printf "%02d" $DATE)

echo "Setting up day $DATE"

touch "tests/day${PADDED_DATE}_1.txt"

curl "https://adventofcode.com/$YEAR/day/${DATE}/input" -H "Cookie: session=$SESSION" >"inputs/day${PADDED_DATE}.txt"

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
