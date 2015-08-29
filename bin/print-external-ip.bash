#!/bin/bash

curl --silent checkip.dyndns.org | sed 's/.*Current IP Address: //; s/<.*$//'

