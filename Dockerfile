FROM mozilla/sbt:8u181_1.2.7 as builder

WORKDIR /gateway

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
