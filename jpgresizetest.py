
import Image
import glob
import os

#Function to resize image, preserving aspect ratio

def resizeAspect(im, size):
    w,h = im.size
    aspect=min(size[0]/float(w), size[1]/float(h))
    return im.resize((int(w*aspect),int(h*aspect)),Image.ANTIALIAS)

# imgList=glob.glob('/Volumes/Untitled/tmp/images/*.png')         #Find all png images in a directory
imgList=glob.glob('C:/Users/Sparks/Desktop/desk/code/github/testproject/jpgtest/jpgtest*.jpg')         # Find all jpg images in a directory

for img in imgList:                              #Loop through all found images
    im = Image.open(img)                     #open the image
    print "resizing:",os.path.basename(img)
    w,h = im.size                                #Get image width and height
    #if min(w,h)<120:
    #Check if either dimension is smaller then 600
    w,h = im.size
    im=resizeAspect(im,(w * 0.57, h * 0.57))            #Re-size Image
    ##w,h = im.size                            #update image size
    ##center = [int(w/2.0),int(h/2.0)]             #Calculate Center
    ##box = (center[0]-60, center[1]-60, center[0]+60, center[1]+60) #Defines a box where you want it to be cropped
    ##croppedIm = im.crop(box)                     #Crop the image
    #croppedIm.show()                            #Show the cropped image
    path, rest = os.path.split(img)
    fileName, fileExtension=os.path.splitext(img)
    im.save("C:/Users/Sparks/Desktop/desk/code/github/testproject/jpgtest"+rest, "JPG")  #Save the cropped image

print "No errors!!!!"
