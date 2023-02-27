# docker build . -f Dockerfile -t issue_poc_runner
FROM ubuntu:jammy

RUN apt-get update && \
    DEBIAN_FRONTEND="noninteractive" apt-get install --no-install-recommends -y \
      cmake make autoconf g++-10 build-essential valgrind \
      ghc ghc-prof cabal-install \
      git ca-certificates \
      libtool libssl-dev pkg-config \
      libgrpc++-dev protobuf-compiler-grpc libprotobuf-dev \
      libboost-dev libboost-coroutine-dev && \
    update-alternatives --install /usr/bin/gcc gcc /usr/bin/gcc-10 20 && \
    update-alternatives --install /usr/bin/g++ g++ /usr/bin/g++-10 20 && \
    rm -rf /var/lib/apt/lists/*

ARG ASIO_GRPC_RELEASE=master
RUN export BUILD_DIR=$(mktemp -d) && cd $BUILD_DIR && \
    git init && git remote add origin https://github.com/Tradias/asio-grpc.git && \
    git fetch --depth 1 origin $ASIO_GRPC_RELEASE && \
    git checkout FETCH_HEAD && \
    cmake -B build -DCMAKE_INSTALL_PREFIX=/usr/local . && \
    cmake --build build --target install && \
    rm -rf $BUILD_DIR

RUN echo "root:toor" | chpasswd
