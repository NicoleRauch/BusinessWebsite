#!/bin/sh

git pull
npm start
cd hakyll
stack build
stack exec site rebuild
