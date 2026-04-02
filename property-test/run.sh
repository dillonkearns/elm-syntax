#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")"

# Build the Elm worker that uses local elm-syntax source
echo "Building worker..."
npm run build:worker

# Run the property test script
echo "Running property tests..."
npx elm-pages run PropertyTest "$@"
