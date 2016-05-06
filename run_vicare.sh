#!/bin/bash

CHURISO_DIR="<put-churiso-dir-here>"

if [ -f ./vicare-docker ]
then
    docker build -t vicare-docker .
fi

docker run -itv $CHURISO_DIR:/churiso vicare-docker
