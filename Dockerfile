FROM openjdk:8u181-jdk-alpine
WORKDIR /home/apps/

RUN set -ex \
    && ln -sf /usr/share/zoneinfo/Asia/Shanghai /etc/localtime && echo 'Asia/Shanghai' >/etc/timezone

COPY web/target/ROOT.jar .

ENV JAVA_OPTS=""

ENTRYPOINT java ${JAVA_OPTS}-Djava.security.egd=file:/dev/./urandom -jar /home/apps/ROOT.jar
