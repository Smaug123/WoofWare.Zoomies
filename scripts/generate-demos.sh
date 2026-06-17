#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

cd "$REPO_ROOT"

# Ensure output directory exists
mkdir -p output/demos

# Build the demos project once
echo "Building WoofWare.Zoomies.Demos..."
dotnet build WoofWare.Zoomies.Demos -c Release

# Run VHS on each .tape file
for tape in docs/demos/*.tape; do
    if [[ -f "$tape" ]]; then
        name=$(basename "$tape" .tape)
        echo "Generating ${name}.gif..."
        vhs "$tape"
    fi
done

echo "Demos generated to output/demos/"
