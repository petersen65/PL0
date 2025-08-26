# Configures the development container for the project
# syntax=docker/dockerfile:1

# Start from a given base image with CPU architecture 'amd64' because the PL0 compiler only supports 'amd64'
ARG ARCHITECTURE
ARG BASE_IMG
FROM --platform=${ARCHITECTURE} ${BASE_IMG}

# Ensure that the system is up to date, and the prerequisites are installed
RUN apt-get update && apt-get install --yes \
    sudo \
    lsb-release \
    curl \
    wget \
    software-properties-common \
    gnupg \
    git \
    gdb \
    shellcheck \
    python3-pip \
    python-is-python3 \
    build-essential

# Install GCC and G++ with a specific version
ARG GCC_VERSION
RUN apt-get install --yes \
    "gcc-${GCC_VERSION}" \
    "g++-${GCC_VERSION}" \
    "gcc-${GCC_VERSION}-locales" \
    "cpp-${GCC_VERSION}-doc" \
    "gcc-${GCC_VERSION}-multilib" \
    "gcc-${GCC_VERSION}-doc" \
    "g++-${GCC_VERSION}-multilib" \
    "libstdc++-${GCC_VERSION}-doc"

# Tools for building software with Go
ARG GO_VERSION
ARG GO_ARCHITECTURE
RUN curl -fsSL -o "/tmp/go${GO_VERSION}.${GO_ARCHITECTURE}.tar.gz" "https://golang.org/dl/go${GO_VERSION}.${GO_ARCHITECTURE}.tar.gz" \
    && tar -C /usr/local -xzf "/tmp/go${GO_VERSION}.${GO_ARCHITECTURE}.tar.gz"

# Install a specific version of LLVM and Clang
ARG LLVM_VERSION
RUN wget https://apt.llvm.org/llvm.sh && \
    chmod +x llvm.sh && \
    ./llvm.sh "${LLVM_VERSION}" all && \
    rm llvm.sh

# Create a non-root user to use if preferred - see https://aka.ms/vscode-remote/containers/non-root-user
ARG TARGET_USER
RUN if [ "${TARGET_USER}" != "root" ] && [ -n "${TARGET_USER}" ]; then \
        useradd -s /bin/bash -m "${TARGET_USER}"; \
        echo "${TARGET_USER} ALL=(ALL) NOPASSWD:ALL" > /etc/sudoers.d/"${TARGET_USER}"; \
        chmod 0440 /etc/sudoers.d/"${TARGET_USER}"; \
    else \
        ln -s /root /home/root; \
    fi

# Switch to the target user
USER ${TARGET_USER}

# Set environment variables for Go
ENV GOPATH=/home/${TARGET_USER}/go
ENV GOROOT=/usr/local/go
ENV PATH=${GOPATH}/bin:${GOROOT}/bin:${PATH}

# Set locale settings to avoid issues with non-UTF-8 characters
ENV LANGUAGE=en_US.UTF-8 \
    LANG=en_US.UTF-8 \
    LC_ALL=en_US.UTF-8

# Switch back to dialog for any ad-hoc use of apt-get
ENV DEBIAN_FRONTEND=dialog

# Install linter for shell programs
RUN /usr/local/go/bin/go install mvdan.cc/sh/v3/cmd/shfmt@latest

# Install Conan C++ package manager and CMake
RUN mkdir --parents "${HOME}/.local/bin" && \
    pip install --break-system-packages --upgrade pip && \
    pip install --break-system-packages conan cmake coloredlogs cmake_format && \
    "${HOME}/.local/bin/conan" profile detect