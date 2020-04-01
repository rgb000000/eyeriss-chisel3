
from store import *
a = np.random.randn(32, 3, 3, 256) *10
a = a.astype(np.int)
filter2mem(a)
from filter_feature import *
b = getW(0, 32)
c = a[0, :, :, 0:64]
d = np.sum(b != c)
print(d)



from store import *
from filter_feature import *
a = np.random.randn(6,6,256)*10
a = a.astype(np.int)
a.shape
feature2mem(a)
b = getFeature(0, 6)
c = np.sum(a[0:5, :, 0:64] != b)
print(c)