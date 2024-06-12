# Configures the development container for the project
# syntax=docker/dockerfile:1

# Start from a given base image
ARG BASE_IMG
FROM ${BASE_IMG}

# Ensure that the system is up to date, and the prerequisites are installed
RUN apt update && apt install -y \
    lsb-release \
    ca-certificates \
    sudo \
    gnupg \
    software-properties-common \
    curl \
    wget \
    git \
    apt-transport-https \
    zip \
    unzip

# Tools for building software with Go
ARG GO_VERSION
RUN curl -fsSL -o /tmp/go${GO_VERSION}.linux-amd64.tar.gz https://golang.org/dl/go${GO_VERSION}.linux-amd64.tar.gz \
    && tar -C /usr/local -xzf /tmp/go${GO_VERSION}.linux-amd64.tar.gz

# Create a non-root user to use if preferred - see https://aka.ms/vscode-remote/containers/non-root-user.
ARG TARGET_USER
RUN useradd -s /bin/bash -m ${TARGET_USER} \
    && echo ${TARGET_USER} ALL=\(root\) NOPASSWD:ALL > /etc/sudoers.d/${TARGET_USER} \
    && chmod 0440 /etc/sudoers.d/${TARGET_USER}

# Switch to the non-root user
USER ${TARGET_USER}

# Set environment variables for Go
ENV GOROOT=/usr/local/go
ENV GOPATH=/home/${TARGET_USER}/go
ENV PATH=$GOPATH/bin:$GOROOT/bin:$PATH

# Set the default shell to bash rather than sh
ENV SHELL /bin/bash

# Switch back to dialog for any ad-hoc use of apt-get
ENV DEBIAN_FRONTEND=dialog
