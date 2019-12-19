#!/bin/bash

sudo apt-get update -y
sudo apt-get install gnupg pbuilder ubuntu-dev-tools apt-file dput libdistro-info-perl fakeroot debhelper -y
pwd
ls -la
ls -la tmp_debian/
dput ppa:janssenlab/software tmp_debian/*xenial*_source.changes
