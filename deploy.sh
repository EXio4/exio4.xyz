#!/bin/bash

function blog() {
  stack exec blog $@ || { echo "Error running command"; exit 1; }
}

blog clean
blog build

rsync -vv -e 'ssh -p443' -a _site/ exio4.xyz:/home/exio4/public_html/blog
