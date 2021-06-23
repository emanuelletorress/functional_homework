produtoEscalar [] [] = 0
produtoEscalar [] (_:_) = 0
produtoEscalar [_] [] = 0
produtoEscalar (_:_:_) [] = 0
produtoEscalar (x:xs) (y:ys) = x*y + produtoEscalar xs ys