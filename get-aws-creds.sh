#!/bin/bash

# Parse command line arguments
while getopts "m:" opt; do
  case ${opt} in
    m ) username=$OPTARG;;
    \? ) echo "Usage: get-aws-creds.sh -m <mfa_device>" >&2
         exit 1;;
    : ) echo "Usage: get-aws-creds.sh -m <mfa_device>" >&2
         exit 1;;
  esac
done

echo "Please enter the MFA token code:"
read mfa_token

# Run `aws sts get-session-token` and save the output to a variable
creds=$(aws sts get-session-token --profile btc --serial-number arn:aws:iam::238974323615:mfa/ilya.tsaryuk@gmail.com --token-code $mfa_token --output json)

# Parse the output and export the credentials as environment variables
export AWS_ACCESS_KEY_ID=$(echo "$creds" | jq -r '.Credentials.AccessKeyId')
export AWS_SECRET_ACCESS_KEY=$(echo "$creds" | jq -r '.Credentials.SecretAccessKey')
export AWS_SESSION_TOKEN=$(echo "$creds" | jq -r '.Credentials.SessionToken')
