#!/bin/sh

git pull
cd setup
npm install
grunt
cd ../jekyll
jekyll build
