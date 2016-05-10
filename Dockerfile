FROM debian:latest

# make sure everything's up to date
ENV DEBIAN_FRONTEND noninteractive
RUN apt-get -y update

# install the dev tools
RUN apt-get install -y apt-utils
RUN apt-get install -y git libtool autoconf make rlwrap texinfo

# install the libraries vicare scheme needs
RUN apt-get install -y libffi-dev libgmp-dev libgsl0-dev

# clone and configure vicare scheme
RUN git clone https://github.com/marcomaggi/vicare.git
WORKDIR vicare/
RUN /bin/sh ./autogen.sh
RUN ./configure --enable-maintainer-mode --enable-sources-installation

# compile and install vicare scheme
RUN make
RUN make install

CMD ["/bin/bash"]
