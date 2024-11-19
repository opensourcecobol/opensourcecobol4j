FROM almalinux:9

SHELL ["/bin/bash", "-c"]

# classpath settings
ENV CLASSPATH :/usr/lib/opensourcecobol4j/libcobj.jar
RUN echo 'export CLASSPATH=:/usr/lib/opensourcecobol4j/libcobj.jar' >> ~/.bashrc

# install dependencies
RUN dnf update -y
RUN dnf install -y epel-release
RUN dnf install -y gcc make bison flex automake autoconf diffutils gettext java-21-openjdk-devel git-clang-format cppcheck libtool gettext-devel unzip
