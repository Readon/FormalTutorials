FROM ghcr.io/readon/spinalhdl:master AS builder

FROM codercom/code-server AS run

ENV DEBIAN_FRONTEND=noninteractive LANG=C.UTF-8 LC_ALL=C.UTF-8 PATH="$PATH:/opt/bin"

USER root

ARG DEPS_RUNTIME="ca-certificates gnupg2 openjdk-11-jdk-headless ccache curl g++ gcc git libtcl8.6 python3 python3-pip python3-wheel libpython3-dev ssh locales make ghdl iverilog libboost1.74-dev"
RUN apt-get update && \
    apt-get install -y --no-install-recommends $DEPS_RUNTIME && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/* && \
    locale-gen en_US.UTF-8
RUN pip install cocotb cocotb-test click

# Add repos and install sbt 
RUN curl -sL "https://keyserver.ubuntu.com/pks/lookup?op=get&search=0x2EE0EA64E40A89B84B2DF73499E82A75642AC823" \
        | gpg2 --dearmour -o /usr/share/keyrings/sdb-keyring.gpg \
    && echo "deb [arch=amd64 signed-by=/usr/share/keyrings/sdb-keyring.gpg] https://repo.scala-sbt.org/scalasbt/debian all main" \
        | tee /etc/apt/sources.list.d/sbt.list \
    && echo "deb [arch=amd64 signed-by=/usr/share/keyrings/sdb-keyring.gpg] https://repo.scala-sbt.org/scalasbt/debian /" \
        | tee /etc/apt/sources.list.d/sbt_old.list \
    && apt update && apt install sbt && rm -rf /var/lib/apt/lists/*
    
COPY --from=builder /opt /opt

USER coder

ARG METALS_VERSION="1.22.0"
RUN code-server --install-extension scalameta.metals@$METALS_VERSION
RUN code-server --install-extension mhutchie.git-graph
RUN code-server --install-extension donjayamanne.githistory

ENV COURSIER_CMD="java -Xmx1G -Dfile.encoding=UTF-8 -jar /home/coder/.local/share/code-server/extensions/scalameta.metals-$METALS_VERSION-universal/coursier"
RUN $COURSIER_CMD install bloop && \
    $COURSIER_CMD install metals

RUN echo "export PATH=\"\$PATH:/opt/bin\"" >> ~/.bashrc

ARG JAVA_EXTRA_OPTS="-Xmx2g -Xms2g"
ENV JAVA_OPTS="${JAVA_OPTS} ${JAVA_EXTRA_OPTS}"
RUN git clone https://github.com/Readon/FormalTutorials.git && \ 
    cd FormalTutorials && \
    git checkout docker && \
    git submodule update --init --recursive && \
    sbt compile && \
    mill _.compile && \
    cd ..
