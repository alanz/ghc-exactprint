
instance ArrowTransformer (AbortT v) where {
  lift = AbortT . (>>> arr Right);
  tmap f = AbortT . f . unwrapAbortT;
};

instance MakeValueTuple Float  where type ValueTuple Float  = Value Float  ; valueTupleOf = valueOf

instance Foo where {
  type ListElement Zero (a,r) = a;
}
