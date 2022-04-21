#FROM openjdk:8u181-jdk-alpine
#WORKDIR /home/apps/
#RUN set -ex \
#    && ln -sf /usr/share/zoneinfo/Asia/Shanghai /etc/localtime && echo 'Asia/Shanghai' >/etc/timezone \
#    && cd /home/apps/ \
#    && mkdir file
#
#RUN apk --update add curl bash ttf-dejavu && \
#      rm -rf /var/cache/apk/*

FROM registry.cn-hangzhou.aliyuncs.com/mscc2020/base_openjdk1.8-mobile-management-system:latest
WORKDIR /home/apps/

COPY core/target/ROOT.jar .

ENV JAVA_OPTS=""

ENTRYPOINT java ${JAVA_OPTS}-Djava.security.egd=file:/dev/./urandom -jar /home/apps/ROOT.jar

#RUN apk add --no-cache tzdata bash  ttf-dejavu fontconfig \


