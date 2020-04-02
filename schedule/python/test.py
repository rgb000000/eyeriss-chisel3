
from store import *
a = np.random.randn(32, 3, 3, 64) *10
a = a.astype(np.int)
filter2mem(a)
from filter_feature import *
b = getW(0, 32)
c = a[0, :, :, 0:64]
d = np.sum(b != c)
print(d)



from store import *
from filter_feature import *
a = np.random.randn(6,6,64)*10
a = a.astype(np.int)
a.shape
feature2mem(a)
b = getFeature(6, 6)
c = np.sum(a[3:, :, 0:64] != b[0:3, :, :])
print(c)