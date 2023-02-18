FROM mcr.microsoft.com/vscode/devcontainers/dotnet:7.0-bullseye

ARG USER='vscode'
ARG WORKDIR='/app'

ENV NVM_DIR="/usr/local/share/nvm"
ENV NVM_SYMLINK_CURRENT=true \
    PATH=${NVM_DIR}/current/bin:${PATH}

# Install Node.js
COPY .devcontainer/node-debian.sh /tmp/library-scripts/

RUN apt-get update && bash /tmp/library-scripts/node-debian.sh "${NVM_DIR}"

WORKDIR ${WORKDIR}

COPY --chown=${USER}:${USER} package*.json ${WORKDIR}

RUN npm install

COPY --chown=${USER} . ${WORKDIR}
