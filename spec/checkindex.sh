#!/bin/bash

# Diff the `tt' entries (identifiers) in the generated index.tex
# (excepting certain special cases like library declarations) with the
# identifiers in the library index, to verify they agree.

diff <(perl -ne 'print if s/^\\item{\\tt{\\?([^{} ]*)}.*$/\1/' index.tex \
        | grep -E -v "^([!',.;?\`]|->|,@|catch|except|export|include(-ci)?|nil|only|prefix|rename|setcar)\$" | sort -u) \
     <(./genstdmod.pl --list < stdmod-raw.tex | sort -u) \
    && echo "Library and identifier indexes are consistent."
