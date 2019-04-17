FROM ubuntu:18.04

RUN apt-get update
RUN apt-get install libssl1.1

RUN mkdir -p /var/www
COPY flack.html /var/www/
COPY flack-app.js /var/www/

RUN mkdir -p /var/www/scripts
COPY scripts/jquery.min.js /var/www/scripts/

RUN mkdir -p /var/www/css
COPY css/flack.css /var/www/css/

RUN mkdir -p /var/www/images
COPY images/flack.png /var/www/images/

COPY flack-1.0.2.tar.gz /root/
RUN tar -xzf /root/flack-1.0.2.tar.gz -C /root/
RUN rm /root/flack-1.0.2.tar.gz

COPY start-flack.sh /
RUN chmod 755 /start-flack.sh

EXPOSE 8080

ENTRYPOINT ["/start-flack.sh"]
