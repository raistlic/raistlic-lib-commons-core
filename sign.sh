#!/bin/bash

mkdir -pv tmp
gcloud secrets versions access latest --secret=settings-maven-central-publish | base64 --decode > tmp/settings.xml
gcloud secrets versions access latest --secret=credentials-maven-central-private-key | base64 --decode | tr , '\n' > tmp/key.pem

MAVEN_USERNAME=$(gcloud secrets versions access latest --secret=credentials-maven-central-username)
MAVEN_PASSWORD=$(gcloud secrets versions access latest --secret=credentials-maven-central-password)
SIGNING_KID=$(gcloud secrets versions access latest --secret=credentials-maven-central-signing-keyid)
SIGNING_PASSWORD=$(gcloud secrets versions access latest --secret=credentials-maven-central-signing-password)

./gradlew clean build sign -x test -Pmaven.username=${MAVEN_USERNAME} -Pmaven.password=${MAVEN_PASSWORD} -Psigning.secretKeyFile=./tmp/key.pem -Psigning.password=${SIGNING_PASSWORD} -Psigning.keyId=${SIGNING_KID}
