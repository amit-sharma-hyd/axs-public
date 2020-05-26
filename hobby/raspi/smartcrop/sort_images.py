#!/usr/bin/env python3
import os, sys, glob
from PIL import Image
from shutil import copyfile
from pathlib import Path

INPUT_DIR = "/home/pi/Pictures/Slides/gphotos/photos/"
OUTPUT_DIR = "/home/pi/Pictures/Slides/feh/"

Path(OUTPUT_DIR + "landscape").mkdir(parents=True, exist_ok=True)
Path(OUTPUT_DIR + "portrait").mkdir(parents=True, exist_ok=True)

print("Processing Dir: " + INPUT_DIR)
os.chdir(INPUT_DIR)
total = 0
portraits = 0
for file in glob.glob(INPUT_DIR + "**/*.jpg", recursive=True):
    print(".", end=" ")
    total = total + 1
    #print("Processing: " + file)
    img = Image.open(file)
    width, height = img.size
    dst = 'landscape'
    if (height > width):
        #print("Image is Portrait")
        dst = 'portrait'
        portraits = portraits + 1
    dst = OUTPUT_DIR + dst
    copyfile(file, dst + '/' + os.path.basename(file))
print("")
print("Completed processing. Moved a total of " + str(total) + 
            " files including " + str(portraits) + " portraits")

