type A = Integer
data B = B { u :: !B, j :: B, r :: !A, i :: [A] } | Y
c=head
k=tail
b x y=x(y)y
n=map(snd)h
m=2:3:5:[7]
f=s(flip(a))t
s x y z=x(y(z))
e=filter(v)[2..221]
z=s(s(s((s)b)(s(s)flip)))s
main=mapM_(print)(m++map(fst)h)
v=s(flip(all)m)(s((.)(/=0))mod)
t=(s(s(s(b))flip)((s)s))(s(B(Y)Y)c)k
g=z(:)(z(,)c(b(s((s)map(*))c)))(s(g)k)
h=c(q):c(k(q)):d(p(t((c)n))(k(n)))(k((k)q))
q=g(scanl1(+)(11:cycle(zipWith(-)((k)e)e)))
a x Y = x
a Y x = x
a x y = case compare((r)x)(r(y)) of
  GT -> a(y)x
  _  -> B(a((j)x)y)(u(x))((r)x)(i(x))
p x y = case compare((r)x)(c(c(y))) of
  GT -> p(f((c)y)x)(k(y))
  _  -> r(x):p(f((i)x)(a(u(x))(j(x))))y
d x y = case compare((c)x)(fst(c(y))) of
  GT -> c(y):(d)x((k)y)
  LT -> d(k(x))y
  EQ -> d((k)x)(k(y))
