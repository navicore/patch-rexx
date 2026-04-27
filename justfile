# Source of truth for build/test/lint.
# Both local dev and GitHub Actions call these recipes — no drift.
# Run `just ci` before pushing; that's exactly what CI runs.

# Default: list available recipes.
default:
    @just --list

# Run all CI checks (same as GitHub Actions).
# This is what developers should run before pushing.
ci: fmt-check lint test build
    @echo "Safe to push to GitHub - CI will pass."

# Format the workspace.
fmt:
    cargo fmt --all

# Verify formatting (CI-friendly; non-zero exit on diff).
fmt-check:
    cargo fmt --all -- --check

# Lint: clippy with warnings promoted to errors, locked Cargo.lock.
lint:
    cargo clippy --locked --workspace --all-targets -- -D warnings

# Run the full test suite.
test:
    cargo test --locked --workspace --all-targets

# Release build of the default binaries.
build:
    cargo build --locked --release

# Install the `rexx` binary into ~/.cargo/bin.
install:
    cargo install --path . --locked --force

# Remove the cargo target/ directory.
clean:
    cargo clean
