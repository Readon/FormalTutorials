FROM ghcr.io/readon/spinalhdl:docker AS base

FROM base AS builder

ENV CODE_SERVER_VERSION=4.12.0
RUN curl -sSOL https://github.com/coder/code-server/releases/download/v${CODE_SERVER_VERSION}/code-server_${CODE_SERVER_VERSION}_amd64.deb && \
    dpkg -i code-server_${CODE_SERVER_VERSION}_amd64.deb

ARG METALS_VERSION="1.23.0"
ARG EXTENSION_DIR=/root/.vscode-server/extensions
RUN mkdir -p $EXTENSION_DIR
RUN /usr/bin/code-server --extensions-dir $EXTENSION_DIR --force --install-extension scalameta.metals@${METALS_VERSION} && \
    /usr/bin/code-server --extensions-dir $EXTENSION_DIR --force --install-extension mhutchie.git-graph && \
    /usr/bin/code-server --extensions-dir $EXTENSION_DIR --force --install-extension donjayamanne.githistory && \
    /usr/bin/code-server --extensions-dir $EXTENSION_DIR --force --install-extension YuTengjing.open-in-external-app

ENV COURSIER_CMD="$EXTENSION_DIR/scalameta.metals-$METALS_VERSION-universal/coursier"
RUN $COURSIER_CMD install bloop:1.5.6 && \
    $COURSIER_CMD install metals:0.11.12

# FROM base AS run

ARG DEPS_RUNTIME="gtkwave"
RUN apt-get update && \
    apt-get install -y --no-install-recommends $DEPS_RUNTIME && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/* && \
    locale-gen en_US.UTF-8

ARG JAVA_EXTRA_OPTS="-Xmx2g -Xms2g"
ENV JAVA_OPTS="${JAVA_OPTS} ${JAVA_EXTRA_OPTS}"
RUN git clone https://github.com/Readon/FormalTutorials.git && \ 
    cd FormalTutorials && \
    git checkout docker && \
    git submodule update --init --recursive && \
    sbt compile && \
    mill _.compile && \
    cd .. && rm -rf FormalTutorials
