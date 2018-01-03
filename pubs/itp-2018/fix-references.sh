#!/bin/sh

sed -i -r "/(url|doi)/ s/\{.?(.)\}/\1/g" ref-mendeley.bib
sed -i -r "s/journaltitle/journal     /g" ref-mendeley.bib
sed -i -r "s/@inbook/@incollection/g" ref-mendeley.bib
sed -i -r "/file/d" ref-mendeley.bib