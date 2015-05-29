#!/usr/bin/env bash

# install required packages
if [[ $(uname -a) == *"Ubuntu"* ]]; then
    export DEBIAN_FRONTEND=noninteractive
     apt-get -q -y build-dep libcurl4-gnutls-dev
     apt-get -q -y install libcurl4-gnutls-dev
     apt-get -q -y install \
        r-base git \
        libboost-dev \
        libboost-system-dev libboost-filesystem-dev libboost-program-options-dev \
        libarpack2-dev liblapack-dev libatlas-dev libopenblas-dev \
        cmake unzip
elif [[ $(ls /etc/*-release) == *"fedora-release"*  ]]; then
     yum -y install \
        R git libcurl libcurl-devel \
        boost-devel \
        boost-system boost-filesystem boost-program-options \
        lapack arpack atlas openblas \
        lapack-devel arpack-devel atlas-devel openblas-devel \
        cmake unzip
elif [[ $(ls /etc/*-release) == *"SuSE-release"*  ]]; then
     zypper -n install \
        R-base libcurl4 libcurl-devel git gcc-c++ \
        boost-devel \
        libboost_system* libboost_filesystem* libboost_program_options* \
        liblapack* lapack-devel \
        libarpack* arpack-ng-devel \
        libatlas3 libatlas3-devel  \
        libopenblas* openblas-devel \
        cmake unzip
else
    echo "Linux distribution not recognized."
    exit
fi

# install armadillo library
cd /tmp
wget http://sourceforge.net/projects/arma/files/armadillo-4.600.4.tar.gz
tar zvxf armadillo-4.600.4.tar.gz
cd armadillo-4.600.4/
cmake .
make
make install
cd /tmp
rm -rf armadillo-4.600.4*

# install bedcoll
cd /tmp
git clone https://kindlychung@bitbucket.org/kindlychung/bedcoll.git
cd bedcoll/
./build.sh
cp bedcoll /usr/local/bin/
cd /tmp
rm -rf bedcoll*

# install PLINK2
cd /tmp
wget https://www.cog-genomics.org/static/bin/plink150209/plink_linux_x86_64.zip
mkdir plink2
unzip plink_linux_x86_64.zip -d plink2
cd plink2
cp -av plink prettify /usr/local/bin
cd /tmp
rm -rf plink*

