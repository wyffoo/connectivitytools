sudo docker build --tag giga-api -f Dockerfile.web .
sudo docker save giga-api > ../dist/giga-api.tar.gz
