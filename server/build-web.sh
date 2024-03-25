docker build --platform linux/amd64 --tag giga-api -f Dockerfile.web . 
sudo docker save giga-api > ../dist/giga-api.tar.gz
#docker build -f Dockerfile.aws --platform linux/amd64 --tag giga . 