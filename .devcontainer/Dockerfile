FROM almalinux:9

SHELL ["/bin/bash", "-o", "pipefail", "-c"]

# classpath settings
ENV CLASSPATH=:/usr/lib/opensourcecobol4j/libcobj.jar

# hadolint ignore=DL3041,DL3003
RUN dnf update -y \
    && dnf install -y --setopt=install_weak_deps=False \
        epel-release \
    && dnf install -y --setopt=install_weak_deps=False \
        gcc \
        make \
        bison \
        flex \
        autoconf \
        diffutils \
        gettext \
        java-21-openjdk-devel \
        git-clang-format \
        cppcheck \
        libtool \
        gettext-devel \
        unzip \
        wget \
    && dnf clean all \
    && rm -rf /var/cache/dnf \
    && wget -q https://ftp.gnu.org/gnu/automake/automake-1.16.5.tar.gz \
    && tar xzf automake-1.16.5.tar.gz \
    && cd automake-1.16.5 \
    && ./configure \
    && make -j"$(nproc)" \
    && make install \
    && cd .. \
    && rm -rf automake-1.16.5 automake-1.16.5.tar.gz

RUN echo 'export CLASSPATH=:/usr/lib/opensourcecobol4j/libcobj.jar' >> ~/.bashrc
