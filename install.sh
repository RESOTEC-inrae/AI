#!/usr/bin/env bash

set -e 
sudo apt update
sudo apt install -y zip libgl1

pip install --upgrade pip
pip install -r requirements.txt
