#!/usr/bin/env bash

set -e 
sudo apt update
sudo apt install -y zip libgl1

pip install --upgrade pip
pip install -r requirements.txt

pip install torch==2.7.1 torchvision==0.22.1 torchaudio==2.7.1 --index-url https://download.pytorch.org/whl/cu118
