#!/bin/sh

sed -i -r "/(url|doi)/ s/\{.?(.)\}/\1/g" ref.bib
sed -i -r "s/journaltitle/journal     /g" ref.bib
