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
