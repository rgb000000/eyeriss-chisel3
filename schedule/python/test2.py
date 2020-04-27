from store import *
W = filterG(8, 3, 64)
filter2mem(W)

F = featureG(8, 64)
feature2mem(F)

from filter_feature import *
readFile("./featureMEM.hex", "./filterMEM.hex")

print("feature_int length", len(feature_int))

print(np.sum(W[0, :, :, :] != getW(0)))

print(np.sum(F[0:5 , :, :] != getFeature(0)))
