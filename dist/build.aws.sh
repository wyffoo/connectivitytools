docker build -f Dockerfile.aws --platform linux/amd64 --tag giga . 
docker save giga > giga.tar.gz
