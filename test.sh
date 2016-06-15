#!/bin/sh

stack --stack-yaml stack-7.10.yaml setup
stack --stack-yaml stack-7.10.yaml test

stack --stack-yaml stack-8.0.yaml setup
stack --stack-yaml stack-8.0.yaml test
