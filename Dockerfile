FROM dcurylo/paket AS build-env

WORKDIR /src
COPY . /src

RUN paket install

RUN msbuild Fulmar/Fulmar.fsproj && msbuild FulmarServer/FulmarServer.fsproj
RUN mkdir ../runtime && cd ../runtime && cp -r ../src/FulmarServer/bin/Debug/* . && cp ../src/FulmarServer/App.config . && cp ../src/FulmarServer/*.html .

FROM fsharp 

COPY --from=build-env /runtime /runtime

WORKDIR /runtime

EXPOSE 8080

ENTRYPOINT mono FulmarServer.exe



