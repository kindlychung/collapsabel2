#!/usr/bin/env bash

proj_dirname=$(basename $(realpath .))
proj_pdf=${proj_dirname}.pdf
rm "$proj_pdf"
cd ../
R CMD Rd2pdf "$proj_dirname"
mv "$proj_pdf" "$proj_dirname"
