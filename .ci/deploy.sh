#!/bin/bash

sudo apt-get update -y
sudo apt-get install gnupg pbuilder ubuntu-dev-tools apt-file libdistro-info-perl fakeroot debhelper dput-ng -y
pwd
ls -la
ls -la tmp_debian/
git stash pop
ls -la ~/.dput.cf
cat ~/.dput.cf
dput janssenlab-ppa tmp_debian/*xenial*_source.changes
