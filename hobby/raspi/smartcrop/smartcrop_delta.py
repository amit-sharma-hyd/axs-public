#!/usr/bin/env python3
import os, sys, glob
import numpy as np
import cv2
from pathlib import Path
from shutil import copyfile
from PIL import Image
import importlib
from smartcrop import *

GPHOTOS_DIR = "/home/pi/Pictures/Slides/gphotos/photos/"
FEH_DIR = "/home/pi/Pictures/Slides/feh/landscape/"

# GPHOTOS_DIR = "/Users/amitsharma/axs/hobby/raspi/images/photos/"
# FEH_DIR = '/Users/amitsharma/axs/hobby/raspi/slideshow/test/output/landscape/'

Path(FEH_DIR + "processed").mkdir(parents=True, exist_ok=True)

print("Checking for new photos...")

gphotos_files = glob.glob(GPHOTOS_DIR + "**/*.jpg", recursive=True)
gphotos_filenames = [os.path.basename(file) for file in gphotos_files]
feh_files = glob.glob(FEH_DIR + "**/*.jpg", recursive=True)
feh_filenames = [os.path.basename(file) for file in feh_files]

print("Found " + str(len(gphotos_files)) + " files in gphotos.")
print("Found " + str(len(feh_files)) + " files in feh.")

# Delete removed pics
to_delete = list(set(feh_filenames) - set(gphotos_filenames))
print("Found " + str(len(to_delete)) + " deleted photos.")
d = 0
print("Deleting removed photos...")
for file in to_delete:
    os.remove(FEH_DIR + file)
    d = d+1
print("Removed " + str(d) + " photos.")

# Process new pics
to_process = list(set(gphotos_filenames) - set(feh_filenames))
print("Found " + str(len(to_process)) + " new photos.")
to_process_files = [f for f in gphotos_files if os.path.basename(f) in to_process]
total = 0
portraits = 0
print("Processing new pics...")
for file in to_process_files:
    total = total + 1
    # Check orientation
    #print("Processing: " + file)
    img = Image.open(file)
    print(".", end=" ")
    width, height = img.size
    if (height > width):
        portraits = portraits + 1
        # loading image
        test_image = cv2.imread(file)
        # crop the image
        crop_img = detect_faces(haar_face_cascade, test_image)
        # Save cropped image
        cv2.imwrite(FEH_DIR + 'processed/' + os.path.basename(file), crop_img)
    else:
        copyfile(file,  FEH_DIR + "/" + os.path.basename(file))
print("")
print("Completed delta processing. Moved a total of " + str(total) + 
            " files including " + str(portraits) + " portraits")