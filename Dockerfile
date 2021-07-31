FROM openjdk:8-jre-slim as builder
ENV DEBIAN_FRONTEND noninteractive
RUN apt-get update && \
    apt-get install -y --no-install-recommends apt-transport-https apt-utils bc dirmngr gnupg && \
    echo "deb https://repo.scala-sbt.org/scalasbt/debian all main" | tee /etc/apt/sources.list.d/sbt.list && \
    echo "deb https://repo.scala-sbt.org/scalasbt/debian /" | tee /etc/apt/sources.list.d/sbt_old.list && \
    apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv 2EE0EA64E40A89B84B2DF73499E82A75642AC823 && \
    apt-get update && \
    apt-get upgrade -y && \
    apt-get install -y --no-install-recommends sbt=1.2.7 wget
WORKDIR /gateway
RUN wget https://github.com/graalvm/graalvm-ce-builds/releases/download/vm-19.3.1/graalvm-ce-java8-linux-amd64-19.3.1.tar.gz && \
    tar -xf graalvm-ce-java8-linux-amd64-19.3.1.tar.gz
ENV JAVA_HOME="/gateway/graalvm-ce-java8-19.3.1"
ENV PATH="${JAVA_HOME}/bin:$PATH"

ADD ["./app", "/gateway/proxy/app"]
ADD ["./conf", "/gateway/proxy/conf"]
ADD ["./project", "/gateway/proxy/project"]
COPY build.sbt /gateway/proxy/

WORKDIR /gateway/proxy/
RUN sbt assembly
RUN mv `find . -name gatewayproxy-*.jar` /gateway-proxy.jar
CMD ["java", "-jar", "/gateway-proxy.jar"]

FROM openjdk:8-jre-slim
RUN adduser --disabled-password --home /home/ergo/ --uid 9052 --gecos "ErgoPlatform" ergo && \
    install -m 0750 -o ergo -g ergo  -d /home/ergo/gateway
COPY --from=builder /gateway-proxy.jar /home/ergo/gateway-proxy.jar
COPY ./conf/application.conf /home/ergo/gateway/application.conf
RUN chown ergo:ergo /home/ergo/gateway-proxy.jar
USER ergo
EXPOSE 9000
WORKDIR /home/ergo
ENTRYPOINT java -jar -D"config.file"=gateway/application.conf /home/ergo/gateway-proxy.jar
