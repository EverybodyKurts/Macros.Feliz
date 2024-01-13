#! /bin/bash
set -e

dotnet tool restore
dotnet paket install
dotnet restore

exec "$@"
