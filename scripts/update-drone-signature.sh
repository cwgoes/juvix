#!/bin/bash

PROFILE=$1
DRONE_URL=${2:-juvix.ci.heliax.dev}

if [ $# -eq 0 ]; then
    echo "No arguments provided. AWS Profile and Drone url needed."
    exit 1
fi

TOKEN=$(aws ssm get-parameter --name "drone_machine_secret" --with-decryption --region eu-west-1 --profile $1 | jq -r '.Parameter.Value')

export DRONE_TOKEN=$TOKEN
export DRONE_SERVER=http://$DRONE_URL

drone sign --save heliaxdev/juvix
