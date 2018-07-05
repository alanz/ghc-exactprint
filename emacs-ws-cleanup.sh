#!/bin/sh

# Remove trailing whitespace, and replace tabs with the appropriate number of spaces

for file in `find . -name "*.hs"`; do
    echo "$file"; emacs --batch -Q "$file" \
        --eval '(setq-default indent-tabs-mode nil)' \
        --eval '(whitespace-cleanup)' \
        --eval '(untabify (point-min) (point-max))' \
        --eval '(perform-replace " " " " nil nil nil)' \
        --eval '(perform-replace "" "" nil nil nil)' \
        -f 'save-buffer'
    fromdos $file
    # Make sure there is a newline at the end of the file
    echo "" >> $file
done
