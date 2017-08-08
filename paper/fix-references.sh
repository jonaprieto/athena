#!/bin/sh

sed -i -r "/(url|doi)/ s/\{.?(.)\}/\1/g" ref.bib