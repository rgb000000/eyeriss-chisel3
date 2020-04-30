import os
import math
env_dist = os.environ

# random data parameters
BITS = 8
VARIANCE = 1

# array parameters
ROWMAX = 5
FILTERSIZE = 3
if env_dist.get("isServer") == "true":
    print("The program is run on server")
    CHANNELMAX = 64
else:
    print("The program is run on PC? 'Please export isServer=true' if run on server")
    CHANNELMAX = 8

STEP = ROWMAX - (FILTERSIZE - 1)

# convolution parameters
outChannel = 1
inChannel = 8
inChannelGroup = math.ceil(inChannel / CHANNELMAX)
featureSize = 5

assert((inChannel % CHANNELMAX) == 0, "inChannel must be multiple CHANNELMAX")

print()
print()
print("### Config Parameters")
print("ROWMAX :         ", ROWMAX)
print("FilterSize :     ", FILTERSIZE)
print("CHANNELMAX :     ", CHANNELMAX)
print("STEP :           ", STEP)
print("OutChannel :     ", outChannel)
print("FeatureSize :    ", featureSize)
print("inChannel :      ", inChannel)
print("inChannelGroup : ", inChannelGroup)
print("### Config Print End")
print()
print()
