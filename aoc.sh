AOC="PATH"
AOC_COOKIE=""

aos-py () {
    cd $AOC;
    if [ $3 ]
    then
        python3 "lib/$1/day$2.py" $3 < "files/$1/day$2.txt"
    elif [ $1 ]
    then
        python3 "lib/$1/day$2.py" < "files/$1/day$2.txt"
    else
        YEAR=$(date +%Y)
        DAY=$(date +%-d)
        python3 "lib/$YEAR/day$DAY.py" < "files/$YEAR/day$DAY.txt"
    fi
}

aos-ocaml () {
    cd $AOC;
    if [ $3 ]
    then
        dune exec bin/main.exe -- -y $1 -d $2 -p $3
    elif [ $1 ]
    then
        dune exec bin/main.exe -- -y $1 -d $2
    else
        YEAR=$(date +%Y)
        DAY=$(date +%-d)
        dune exec bin/main.exe -- -y $YEAR -d $DAY
    fi
}

aos () {
    echo "Python (PROD)"
    aos-py "$@"
    echo
    echo "OCaml (PROD)"
    aos-ocaml "$@"
}

aot-py () {
    cd $AOC;
    if [ $3 ]
    then
        python3 "lib/$1/day$2.py" $3 < "test.txt"
    elif [ $1 ]
    then
        python3 "lib/$1/day$2.py" < "test.txt"
    else
        YEAR=$(date +%Y)
        DAY=$(date +%-d)
        python3 "lib/$YEAR/day$DAY.py" < "test.txt"
    fi
}

aot-ocaml () {
    cd $AOC;
    if [ $3 ]
    then
        dune exec bin/main.exe -- -y $1 -d $2 -p $3 -f "test.txt"
    elif [ $2 ]
    then
        dune exec bin/main.exe -- -y $1 -d $2 -f "test.txt"
    else
        YEAR=$(date +%Y)
        DAY=$(date +%-d)
        dune exec bin/main.exe -- -y $YEAR -d $DAY -f "test.txt"
    fi
}

aot () {
    echo "Python (TEST)"
    aot-py "$@"
    echo
    echo "OCaml (TEST)"
    aot-ocaml "$@"
}

setup () {
    cd $AOC;
    if [ $1 ]
    then
        mkdir -p "files/$1"
        curl --cookie "session=$AOC_COOKIE" https://adventofcode.com/$1/day/$2/input > files/$1/day$2.txt

        mkdir -p "lib/$1"
        if [ ! -f "lib/$1/day$2.ml" ]; then
            cp template.ml "lib/$1/day$2.ml"
        fi

        if [ ! -f "lib/$1/day$2.py" ]; then
            cp template.py "lib/$1/day$2.py"
        fi
    else
        YEAR=$(date +%Y)
        DAY=$(date +%-d)
        mkdir -p "files/$YEAR"
        curl --cookie "session=$AOC_COOKIE" "$(echo `date +https://adventofcode.com/%Y/day/%d/input` | sed 's/\/0/\//g')" > files/$YEAR/day$DAY.txt

        mkdir -p "lib/$YEAR"
        touch "lib/$YEAR/day$DAY.ml"

        if [ ! -f "lib/$YEAR/day$DAY.ml" ]; then
            cp template.ml "lib/$YEAR/day$DAY.ml"
        fi

        if [ ! -f "lib/$YEAR/day$DAY.py" ]; then
            cp template.py "lib/$YEAR/day$DAY.py"
        fi
    fi
}
