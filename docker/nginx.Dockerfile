FROM ubuntu:18.04

RUN apt-get update
RUN apt-get install -y nginx

COPY nginx.conf /etc/nginx/nginx.conf

EXPOSE 8080

ENTRYPOINT ["nginx", "-g", "daemon off;"]
