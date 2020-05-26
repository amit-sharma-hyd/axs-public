
import os, sys, glob
import numpy as np
import cv2
from pathlib import Path

TARGET_ASP_RATIO = 16.0/9
haar_face_cascade = cv2.CascadeClassifier('./haarcascade_frontalface_default.xml')
# haar_face_cascade = cv2.CascadeClassifier('./haarcascade_glasses.xml')
haar_eye_cascade = cv2.CascadeClassifier('./haarcascade_eye.xml')


def crop_to_first_face(image_copy, faces_rect, debug=False):
    # Logic to detect cropping area
    y_crop = 0;
    crop_height = int(image_copy.shape[1]*1.0/TARGET_ASP_RATIO)
    if debug:
        print("crop_height: " + str(crop_height))
    # Pick the first face from top and create boundary
    if (len(faces_rect) > 0):
        sorted_faces_rect = sorted(faces_rect , key = lambda x: float(x[1]))
        
        x, y, w, h = sorted_faces_rect[0]
        if debug:
            print("Face Dims: ")
            print(x,y,w,h)
        # Start crop from double the face height
        yc = int(y - int(0.5*h))
        if yc < 0:
            yc = 0;
        y_crop = yc
        if debug:
            print("Crop y positions: ")
            print(y_crop, y_crop+crop_height)
        # Create a border to highlight image cropping
        cv2.rectangle(image_copy, (0, y_crop), 
                      (image_copy.shape[1], y_crop+crop_height), 
                      (0, 0, 250), 5)
        # Crop the image
        crop_img = image_copy[y_crop:y_crop+crop_height, 0:image_copy.shape[1]]
        if debug:
            print("crop_img: ")
            print(crop_img.shape)
    else:
        # no faces found, rotate the image
        crop_img = cv2.rotate(image_copy, cv2.ROTATE_90_CLOCKWISE)
        cv2.rectangle(crop_img, (0,0), (crop_img.shape[1], crop_img.shape[0]),
                      (0, 250, 0), 5)
        

    return crop_img


def detect_faces(cascade, test_image, debug=False):
    # create a copy of the image to prevent any changes to the original one.
    image_copy = test_image.copy()

    #convert the test image to gray scale as opencv face detector expects gray images
    gray_image = cv2.cvtColor(image_copy, cv2.COLOR_BGR2GRAY)

    # Applying the haar classifier to detect faces
    faces_rect = cascade.detectMultiScale(gray_image, scaleFactor=1.1, 
                                          minNeighbors=10, minSize=(100,100))

    if debug:
        print("faces_rect: " + str(len(faces_rect)))

#     for (x, y, w, h) in faces_rect:
#         cv2.rectangle(image_copy, (x, y), (x+w, y+h), (255, 0, 0), 2)    
    
    crop_img = crop_to_first_face(image_copy, faces_rect, debug)
    
#     crop_to_most_faces(image_copy, faces_rect)
    
    return crop_img

def convertToRGB(image):
    return cv2.cvtColor(image, cv2.COLOR_BGR2RGB)
