sudo docker build --tag giga-calc-worker -f Dockerfile.calc .
sudo docker save giga-calc-worker > ../dist/giga-calc-worker.tar.gz
