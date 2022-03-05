{-# LANGUAGE RankNTypes #-}
module Canonical.Shared where
import           PlutusTx.Prelude
import           PlutusTx

#define DEBUG

#if defined(DEBUG)
#define TRACE_IF_FALSE(a,b) traceIfFalse a b
#define TRACE_ERROR(a) traceError a
#define FROM_BUILT_IN_DATA(m, a) case fromBuiltinData a :: Maybe a of { Nothing -> TRACE_ERROR(m); Just x -> x }
#define DataConstraint(a) FromData a
#else
#define TRACE_IF_FALSE(a,b) b
#define TRACE_ERROR(a) error ()
#define FROM_BUILT_IN_DATA(m, a) unsafeFromBuiltinData a :: a
#define DataConstraint(a) UnsafeFromData a
#endif

wrap  :: forall a b c .
            ( DataConstraint(a)
            , DataConstraint(b)
            , DataConstraint(c)
            )
      => (a -> b -> c -> Bool)
      -> BuiltinData
      -> BuiltinData
      -> BuiltinData
      -> ()
wrap f a b c
  = check
    ( f
        ( FROM_BUILT_IN_DATA("datum failed", a))
        ( FROM_BUILT_IN_DATA("redeemer failed", b))
        ( FROM_BUILT_IN_DATA("script context failed", c))
    )
