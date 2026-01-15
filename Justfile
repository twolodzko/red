test:
    cargo clippy
    cargo test

build:
    cargo build --release
    cp ./target/release/red ./

install:
    cargo install --path .

help:
    cargo run -- --help

benchmark: build
    #!/bin/bash
    set -euo pipefail

    bench() {
        if ! diff -q <(eval "$2") <(eval "$3") ; then
            echo "outputs of $2 and $3 differ"
            exit 1
        fi
        hyperfine -r "$1" --shell=none -w 5 "$2" "$3"
    }

    bench 1000 \
        'awk "{ print \$0 }" README.md' \
        './red "print(.)" README.md'

    bench 1000 \
        'awk "/red/" README.md' \
        './red ". =~ /red/, print(.)" README.md'

    bench 1000 \
        'awk "/match\(\)/, /### Templates/" README.md' \
        './red "between(/match\(\)/, /### Templates/), print(.)" README.md'

    bench 1000 \
        'awk "{ if (length(\$0) > max) max = length(\$0) } END { print max }" README.md' \
        './red "var &max = 0; if (len(.) > &max) &max = len(.); end { print(&max) }" README.md'

    bench 1000 \
        'awk "{ a[i++] = $0 } END { for (j=i-1; j>=0;) print a[j--] }" README.md' \
        './red "var &a = []; &a += .; end { print(join(rev(&a), "\n")) }" README.md'
