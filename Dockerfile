FROM mcr.microsoft.com/vscode/devcontainers/dotnet:8.0-bookworm-slim

ARG USER='vscode'
ARG WORKDIR='/app'
ARG NODE_VERSION="lts/*"

ENV PATH=$PATH:/home/vscode/.dotnet:/home/vscode/.dotnet/tools \
    DEBIAN_FRONTEND=noninteractive

RUN if [ "${NODE_VERSION}" != "none" ]; then su vscode -c "umask 0002 && . /usr/local/share/nvm/nvm.sh && nvm install ${NODE_VERSION} 2>&1"; fi

# Install Node.js
COPY docker-entrypoint.sh /usr/local/bin/docker-entrypoint.sh

RUN apt-get update

WORKDIR ${WORKDIR}

COPY --chown=${USER}:${USER} package.json yarn.lock ${WORKDIR}

RUN  su vscode -c "source /usr/local/share/nvm/nvm.sh && npm install -g yarn" 2>&1

COPY --chown=${USER} . ${WORKDIR}

RUN dotnet tool restore && \
    dotnet paket install && \
    dotnet restore

ENTRYPOINT ["docker-entrypoint.sh"]

EXPOSE 5173

CMD ["yarn", "start"]