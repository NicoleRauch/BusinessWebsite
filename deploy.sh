#!/bin/sh

git pull
cd setup
grunt
cd ../jekyll
jekyll build
