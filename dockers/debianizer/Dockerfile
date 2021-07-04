FROM ubuntu:18.04

ENV LANG=C.UTF-8        \
    LC_ALL=C.UTF-8

ARG EULER_UID
ARG EULER_GID
ARG STACK_RESOLVER
ARG apt_install="apt install --no-install-recommends --yes"
ARG PACKAGE_NAME=ff
ARG USER=ff
ARG GROUP=ff
ARG HOME=/home/$USER
ARG REPO=$HOME/ff-repo
ARG TMP_BIN=$HOME/temp
ARG BUILD_DIRECTORY=$HOME/ff-deb
ARG DEB=$HOME/deb
ARG BIN=$BUILD_DIRECTORY/opt/$PACKAGE_NAME

RUN \
# Check for mandatory build arguments
    : "${EULER_UID:?mandatory build argument is missing}" \
    : "${EULER_GID:?mandatory build argument is missing}" \
    : "${STACK_RESOLVER:?mandatory build argument is missing}"

RUN groupadd -g ${EULER_GID} $GROUP
RUN useradd -m -u ${EULER_UID} -g ${EULER_GID} $USER

RUN mkdir -p $HOME/.stack
RUN mkdir -p $TMP_BIN
RUN mkdir -p $REPO
RUN mkdir -p $BUILD_DIRECTORY
RUN mkdir -p $BIN
RUN mkdir -p $DEB
RUN chown -R ${EULER_UID}:${EULER_GID} $HOME/

RUN apt update
RUN $apt_install lintian fakeroot debhelper git curl tree \
  g++ libtinfo-dev zlib1g-dev qt5-default ca-certificates libgmp-dev

WORKDIR $HOME
RUN tree

RUN curl -sSL https://get.haskellstack.org/ | sh

RUN git clone https://github.com/willbasky/ff.git $REPO
RUN chown -R ${EULER_UID}:${EULER_GID} $HOME/

RUN tree

WORKDIR $BUILD_DIRECTORY
COPY ./ff-deb .

# WORKDIR $HOME
# RUN tree

WORKDIR $REPO
RUN stack --allow-different-user --local-bin-path=$TMP_BIN/ install $PACKAGE_NAME

WORKDIR $HOME
RUN cp $TMP_BIN/$PACKAGE_NAME $BIN

RUN tree

WORKDIR $BUILD_DIRECTORY
RUN dpkg-buildpackage -rfakeroot -b -us -uc -d

WORKDIR $HOME
RUN lintian --suppress-tags dir-or-file-in-opt,embedded-library *.deb

RUN cp *.deb $DEB
