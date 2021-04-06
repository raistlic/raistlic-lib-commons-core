#!/bin/bash

set -ex

mkdir -pv tmp
gcloud secrets versions access latest --secret=settings-maven-central-publish | base64 --decode > tmp/settings.xml
gcloud secrets versions access latest --secret=credentials-maven-central-private-key > tmp/key.pem

MAVEN_USERNAME=$(gcloud secrets versions access latest --secret=credentials-maven-central-username)
MAVEN_PASSWORD=$(gcloud secrets versions access latest --secret=credentials-maven-central-password)
SIGNING_KID=$(gcloud secrets versions access latest --secret=credentials-maven-central-signing-keyid)
SIGNING_PASSWORD=$(gcloud secrets versions access latest --secret=credentials-maven-central-signing-password)

echo "maven username: $MAVEN_USERNAME"
echo "maven password: $MAVEN_PASSWORD"
echo "signing kid: $SIGNING_KID"
echo "signing password: $SIGNING_PASSWORD"
echo "signing key: $(cat tmp/key.pem)"

./gradlew clean build uploadArchives -Pmaven.username=${MAVEN_USERNAME} -Pmaven.password=${MAVEN_PASSWORD} -Psigning.secretKeyRingFile=./tmp/key.pem -Psigning.password=${SIGNING_PASSWORD} -Psigning.keyId=${SIGNING_KID}
