#!/bin/sh

# Remove trailing whitespace, and replace tabs with the appropriate number of spaces

for file in `find . -name "*.hs"`; do
    echo "$file"; emacs --batch "$file" \
        --eval '(setq-default indent-tabs-mode nil)' \
        --eval '(whitespace-cleanup)' \
        --eval '(untabify (point-min) (point-max))' \
        -f 'save-buffer'
    fromdos $file
    # Make sure there is a newline at the end of the file
    echo "" >> $file
done
