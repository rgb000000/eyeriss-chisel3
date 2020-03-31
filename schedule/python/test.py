
from store import *
a = np.random.randn(32, 3, 3, 32) *10
a = a.astype(np.int)
filter2mem(a)
from filter_feature import *
b = getW(0, 32, 32)
b = b[:, :, 0:32]
c = a[0, :, :, :]
d = np.sum(b != c)
print(d)