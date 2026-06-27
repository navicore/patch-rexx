# Source of truth for build/test/lint.
# Both local dev and CI (Forgejo Actions) call these recipes — no drift.
# Run `just ci` before pushing; that's exactly what CI runs.

# Default: list available recipes.
default:
    @just --list

# Run all CI checks (same as the CI workflow).
# This is what developers should run before pushing.
ci: fmt-check lint test build
    @echo "Safe to push - CI will pass."

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

# Quick code stats: LOC, largest files, module tree.
# Requires `scc` (brew install scc · or · cargo install scc) and
# `cargo-modules` (cargo install cargo-modules) — both are dev-only,
# not in Cargo.toml. The cargo-modules call falls back to a hint if
# the tool is missing so the recipe still prints something useful.
stats:
    @echo "=== Workspace LOC ==="
    @scc src --no-cocomo
    @echo ""
    @echo "=== Largest Rust source files (top 15) ==="
    @scc src --by-file --no-cocomo -s lines -i rs | head -20
    @echo ""
    @echo "=== Module tree ==="
    @cargo modules structure --lib 2>/dev/null \
      || echo "(install cargo-modules for the module tree: cargo install cargo-modules)"

# Regenerate docs/README.md from the repo-root README.md (header-flattened for
# the mdBook sidebar). Run before any mdbook build.
gen-docs:
    ./scripts/generate-examples-docs.sh

# Build the mdBook site into ./book/ (what the docs.yml workflow runs).
docs: gen-docs
    mdbook build

# Serve the docs locally with hot reload.
docs-serve: gen-docs
    mdbook serve --open
