# monad-var

The aim of this library is to provide a unified set of operations (`new`, `read`, `write`, `swap`, various strict `mutate*` functions and their derivatives (like mutate-and-return-the-old-value) and a certain interface for `MVar`-like things) over all common variable types: `IORef`, `STRef s`, `MVar`, `TVar`, `TMVar`.

When possible type classes are generalized to be over abstract containers rather than variables: e.g. it'd make sense to implement `MonadRead TChan` and `MonadMutate_ Vector` instances, but no such instances are provided right now.

For details see the [announce](https://github.com/effectfully-ou/sketches/tree/master/ann-monad-var).
