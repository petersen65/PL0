# Configures the development container for the project
# syntax=docker/dockerfile:1

# Start from a given base image with CPU architecture 'amd64' because the PL0 compiler only supports 'amd64'
ARG ARCHITECTURE=linux/amd64
ARG BASE_IMG=ubuntu:latest
FROM --platform=${ARCHITECTURE} ${BASE_IMG}

# Ensure that the system is up to date, and the prerequisites are installed
RUN apt update && apt install --yes \
    curl \
    git \
    shellcheck

# Tools for building software with Go
ARG GO_VERSION
RUN curl -fsSL -o /tmp/go${GO_VERSION}.linux-amd64.tar.gz https://golang.org/dl/go${GO_VERSION}.linux-amd64.tar.gz \
    && tar -C /usr/local -xzf /tmp/go${GO_VERSION}.linux-amd64.tar.gz

# Install a specific version of GCC
ARG GCC_VERSION
RUN apt-get update && apt-get install --yes \
    gcc-${GCC_VERSION} \
    g++-${GCC_VERSION} 

# Install a specific version of LLVM and Clang
ARG LLVM_VERSION
RUN wget https://apt.llvm.org/llvm.sh && \
    chmod +x llvm.sh && \
    ./llvm.sh "${LLVM_VERSION}" all && \
    rm llvm.sh

# Create a non-root user to use if preferred - see https://aka.ms/vscode-remote/containers/non-root-user
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

# Switch back to dialog for any ad-hoc use of apt-get
ENV DEBIAN_FRONTEND=dialog

# Install linter for shell programs
RUN go install mvdan.cc/sh/v3/cmd/shfmt@latest
