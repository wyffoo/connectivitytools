#!/bin/bash
HOST='root@157.230.18.82'
#scp ./dist/giga.tar.gz $HOST:/opt/
#scp ./dist/giga-api.tar.gz $HOST:/opt/
#scp ./dist/giga-calc-worker.tar.gz $HOST:/opt/

#Copy just code
cd ./dist/client
tar -zcvf ../../client.tar.gz .
cd ../..
cd ./server
tar -zcvf ../server.tar.gz .
cd ..
scp client.tar.gz $HOST:/opt
sleep 5
scp server.tar.gz $HOST:/opt

### Execute remotely
#Prepare app
#sleep 5
ssh $HOST 'rm -Rf /opt/server;mkdir /opt/server;tar -xf /opt/server.tar.gz -C /opt/server'
#sleep 5
ssh $HOST 'rm -Rf /opt/client;mkdir /opt/client;tar -xf /opt/client.tar.gz -C /opt/client'
#
# Prepare FW
#sleep 5
ssh $HOST 'iptables -F INPUT'
#sleep 5
ssh $HOST 'iptables -I INPUT -p tcp --dport 8000 -i eth0 -j DROP'
#sleep 5
ssh $HOST 'iptables -I INPUT -p tcp --dport 8000 -i eth0 -j DROP'
#sleep 5
ssh $HOST 'iptables -I INPUT -p tcp --dport 9000 -i eth0 -j DROP'
#sleep 5
ssh $HOST 'iptables -I INPUT -p tcp --dport 9000 -i eth1 -j DROP'
##
##GIGA Web Client
ssh $HOST 'docker container restart giga'
#ssh $HOST 'docker container rm giga'
#ssh $HOST 'docker image rm giga:latest'
#ssh $HOST 'docker load -i /opt/giga.tar.gz'
#ssh $HOST 'docker run --env ENV=prod --network=host --name giga --restart=always -itd --mount type=bind,source=/opt/client,target=/var/www/html/public giga:latest'
##ssh $HOST 'docker run --env ENV=prod --network=host --name giga --restart=always -itd giga:latest'
##
###GIGA Backend API
ssh $HOST 'docker container restart giga-api'
#ssh $HOST 'docker container rm giga-api'
#ssh $HOST 'docker image rm giga-api:latest'
#ssh $HOST 'docker load -i /opt/giga-api.tar.gz'
#ssh $HOST 'docker run --env ENV=prod --network=host --name giga-api --restart=always -itd --mount type=bind,source=/opt/server,target=/var/www/html giga-api:latest'
##ssh $HOST 'docker run --env ENV=prod --network=host --name giga-api --restart=always -itd giga-api:latest'
##
#
#if [ "$1" = "withcalc" ]; then
##GIGA Calculation worker
ssh $HOST 'docker container restart giga-calc-worker1'
ssh $HOST 'docker container restart giga-calc-worker2'
ssh $HOST 'docker container restart giga-calc-worker3'
#ssh $HOST 'docker container rm giga-calc-worker1'
#ssh $HOST 'docker container rm giga-calc-worker2'
#ssh $HOST 'docker container rm giga-calc-worker3'
#ssh $HOST 'docker image rm giga-calc-worker:latest'
#ssh $HOST 'docker load -i /opt/giga-calc-worker.tar.gz'
#ssh $HOST 'docker run --env ENV=prod --network=host --name giga-calc-worker1 --restart=always -itd --mount type=bind,source=/opt/server,target=/opt/calculations giga-calc-worker:latest'
#ssh $HOST 'docker run --env ENV=prod --network=host --name giga-calc-worker2 --restart=always -itd --mount type=bind,source=/opt/server,target=/opt/calculations giga-calc-worker:latest'
#ssh $HOST 'docker run --env ENV=prod --network=host --name giga-calc-worker3 --restart=always -itd --mount type=bind,source=/opt/server,target=/opt/calculations giga-calc-worker:latest'
#
#fi

docker run --env ENV=dev --network=host --name giga-calc-worker --restart=always -itd --mount type=bind,source=/run/media/itsaryuk/Projects/bbcon-ui/server,target=/opt/calculations giga-calc-worker:latest
