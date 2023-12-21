#!/bin/bash
HOST=/root@157.230.18.82/
#scp ./dist/giga.tar.gz $HOST:/opt/
#scp ./dist/giga-api.tar.gz $HOST:/opt/
#scp ./dist/giga-calc-worker.tar.gz $HOST:/opt/

#Copy just code
#cd ./dist/client
#tar -zcvf ../../client.tar.gz .
#cd ../..
#cd ./server
#tar -zcvf ../server.tar.gz .
#cd ..
#scp client.tar.gz $HOST:/opt
#sleep 5
#scp server.tar.gz $HOST:/opt

### Execute remotely
#Prepare app
rm -Rf /opt/server;mkdir /opt/server;tar -xf /opt/server.tar.gz -C /opt/server/
rm -Rf /opt/client;mkdir /opt/client;tar -xf /opt/client.tar.gz -C /opt/client/
#
# Prepare FW
sleep 5
iptables -F INPUT; \
iptables -I INPUT -p tcp --dport 8000 -i eth0 -j DROP; \
iptables -I INPUT -p tcp --dport 8000 -i eth0 -j DROP; \
iptables -I INPUT -p tcp --dport 9000 -i eth0 -j DROP; \
iptables -I INPUT -p tcp --dport 9000 -i eth1 -j DROP;
#
#GIGA Web Client
docker container stop giga;\
docker container rm giga;\
docker image rm giga:latest;\
docker load -i /opt/giga.tar.gz;\
docker run --env ENV=prod --network=host --name giga --restart=always -itd --mount type=bind,source=/opt/client,target=/var/www/html/public giga:latest;\
#docker run --env ENV=prod --network=host --name giga --restart=always -itd giga:latest;
#
##GIGA Backend API
docker container stop giga-api;\
docker container rm giga-api;\
docker image rm giga-api:latest;\
docker load -i /opt/giga-api.tar.gz;\
docker run --env ENV=prod --network=host --name giga-api --restart=always -itd --mount type=bind,source=/opt/server,target=/var/www/html giga-api:latest
#

if [ "$1" = "withcalc" ]; then
#GIGA Calculation worker
docker container stop giga-calc-worker1;\
docker container stop giga-calc-worker2;\
docker container stop giga-calc-worker3;\
docker container rm giga-calc-worker1;\
docker container rm giga-calc-worker2;\
docker container rm giga-calc-worker3;\
docker image rm giga-calc-worker:latest;\
docker load -i /opt/giga-calc-worker.tar.gz;\
docker run --env ENV=prod --network=host --name giga-calc-worker1 --restart=always -itd --mount type=bind,source=/run/media/itsaryuk/Projects/giga/server,target=/opt/calculations giga-calc-worker:latest;
docker run --env ENV=prod --network=host --name giga-calc-worker2 --restart=always -itd --mount type=bind,source=/opt/server,target=/opt/calculations giga-calc-worker:latest
#docker run --env ENV=prod --network=host --name giga-calc-worker3 --restart=always -itd --mount type=bind,source=/opt/server,target=/opt/calculations giga-calc-worker:latest

fi
