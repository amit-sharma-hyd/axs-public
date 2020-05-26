#!/usr/bin/env python3
import os, sys, glob
import numpy as np
import cv2
from pathlib import Path
from smartcrop import *


INPUT_DIR = '/Users/amitsharma/axs/github/axs/hobby/raspi/feh/portrait/'
OUTPUT_DIR = '/Users/amitsharma/axs/github/axs/hobby/raspi/feh/processed/'

Path(OUTPUT_DIR).mkdir(parents=True, exist_ok=True)

for file in glob.glob(INPUT_DIR + "*.jpg"):
    #print("Processing: " + INPUT_DIR + file)
    print(".", end=" ")
    # loading image
    test_image = cv2.imread(file)
    # crop the image
    crop_img = detect_faces(haar_face_cascade, test_image)
    # Save cropped image
    cv2.imwrite(OUTPUT_DIR + '/' + os.path.basename(file), crop_img)

print("")
print("Completed processing.")