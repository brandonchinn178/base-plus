# base-plus

A safer replacement for `base`.

## Quickstart

1. Replace `base` with `base-plus` in your `package.yaml` or `.cabal` file

1. If using `hpack`, ignore the `Paths_` module:

    ```hs
    library:
      when:
        - condition: false
          other-modules: Paths_your_library_name
    ```

After these steps, most of your codebase should "just work", aside from changes that will need to be made due to the below changes.

## Why??

`base` has a lot of unsafe design choices that are kept for backwards compatibility. With the benefit of a LOT of hindsight, we can make a better API more befitting of the current era of programming, if you don't happen to care about backwards compatibility. So `base-plus` is here to provide a view into what `base` could look like in the ideal future, an experimental playground if you will.

A lot of existing work has been done in libraries like `relude` to provide safer APIs, but `relude` only replaces `Prelude` and intentionally avoids "Rewrit[ing] `base` from the ground up".

### Design goals

* Improved types
* No partial functions
* Better exception tracking + handling
* Only depends on `base`

## Differences from `base`

### New `IO` and `IOE` types

By far the biggest change in this library is the new `IO` and `IOE` types:

* `IO a` represents a computation that can interact with the real world, but **will not throw an exception**
* `IOE e a` represents a computation that can interact with the real world and can throw exceptions of type `e`

`IO a` in `base` corresponds to `IOE SomeException a` in `base-plus`. Note that this only applies to synchronous, precise exceptions (i.e. exceptions thrown with `throwIO`); impure exceptions (i.e. exceptions thrown with `throw`) and asynchronous exceptions (i.e. exceptions thrown with `throwTo`) are _still not_ captured in `IOE`. Impure exceptions (as usual) should only be used to indicate unrecoverable errors.

Functions like `catch` and `try` will only work on synchronous exceptions by default; to catch async exceptions, use the `*SyncOrAsync` variants.

### Removal of partial functions

Partial functions have been removed, with the exception of `error` and `undefined`. At call sites previously using partial functions, you should explicitly pattern match and call `error` yourself with a helpful error message.

### Improved types

* `Prelude` contains general functions (e.g. `null :: Foldable t => t a -> Bool`) while modules are monomorphized (e.g. `Data.List.null :: [a] -> Bool`) ([Upstream ticket](https://github.com/haskell/core-libraries-committee/issues/22))
* New `Foldable1` type class for functions that are total on non-empty structures
