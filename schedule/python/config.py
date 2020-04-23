import os
env_dist = os.environ

BITS = 8
VARIANCE = 1

ROWMAX = 5
FILTERSIZE = 3
if env_dist.get("isServer") == "true":
    print("The program is run on server")
    CHANNELMAX = 64
else:
    print("The program is run on PC? 'Please export isServer=true' if run on server")
    CHANNELMAX = 8

STEP = ROWMAX - (FILTERSIZE - 1)
