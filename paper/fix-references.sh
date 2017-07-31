#!/bin/sh

sed -i.backup -r "/(url|doi)/ s/\{.?(.)\}/\1/g" ref.bib