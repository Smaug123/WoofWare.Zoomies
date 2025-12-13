#!/usr/bin/env bash
# Concatenate all documentation into a single markdown file for LLM consumption

set -euo pipefail

OUTPUT="output/llm-docs.md"

echo "# WoofWare.Zoomies Documentation (Concatenated)" > "$OUTPUT"
echo "" >> "$OUTPUT"
echo "This is a concatenated version of all documentation for LLM consumption." >> "$OUTPUT"
echo "" >> "$OUTPUT"

# Concatenate in logical order
for dir in how_to explanation architecture; do
  if [ -d "docs/$dir" ]; then
    # Capitalise the directory name for the header
    header=$(echo "$dir" | tr '_' ' ' | awk '{for(i=1;i<=NF;i++){$i=toupper(substr($i,1,1)) substr($i,2)}}1')
    echo "## $header" >> "$OUTPUT"
    echo "" >> "$OUTPUT"
    for f in docs/"$dir"/*.md; do
      if [ -f "$f" ]; then
        # Strip YAML front matter and concatenate
        sed '1{/^---$/!b}; 1,/^---$/d' "$f" >> "$OUTPUT"
        echo -e "\n---\n" >> "$OUTPUT"
      fi
    done
  fi
done

echo "Generated $OUTPUT"
