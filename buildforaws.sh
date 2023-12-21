#aws sts get-session-token --profile btc --serial-number arn:aws:iam::238974323615:mfa/ilya.tsaryuk@gmail.com --token-code ---
#export AWS_ACCESS_KEY_ID=
#export AWS_SECRET_ACCESS_KEY=
#export AWS_SESSION_TOKEN=
gulp build
cd ./dist
bash build.aws.sh
cd ../server
bash build-web.sh
bash build-calc.sh
cd ..
aws ecr get-login-password --region eu-central-1 | sudo docker login --username AWS --password-stdin 238974323615.dkr.ecr.eu-central-1.amazonaws.com
sudo docker tag giga:latest 238974323615.dkr.ecr.eu-central-1.amazonaws.com/connectivitytools:giga-new
sudo docker tag giga-api:latest 238974323615.dkr.ecr.eu-central-1.amazonaws.com/connectivitytools:giga-new-api
sudo docker tag giga-calc-worker:latest 238974323615.dkr.ecr.eu-central-1.amazonaws.com/connectivitytools:giga-new-calc-worker
sudo docker push 238974323615.dkr.ecr.eu-central-1.amazonaws.com/connectivitytools:giga-new
sudo docker push 238974323615.dkr.ecr.eu-central-1.amazonaws.com/connectivitytools:giga-new-api
sudo docker push 238974323615.dkr.ecr.eu-central-1.amazonaws.com/connectivitytools:giga-new-calc-worker
