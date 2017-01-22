# wmonad

wmonad is a tiling window manager based heavily on [xmonad](http://xmonad.org).
I have some very specific ideas about how I would like my windows to be managed.
Realizing this vision in a satisfying way within xmonad's extension model wouldn't be much easier than writing a new window manager.
wmonad is an in-progress attempt at the latter.

Many aspects of wmonad's design will come directly from xmonad.
However, it will differ in the following ways:

-   The `Stack` and `Layout` will be replaced with a less flexible tree.
    Branches in this tree of windows can be stacked, or organized horizontally or vertically.
    Cursors marking branches will allow the user to control groups of windows that are not focused, or even on a different screen.

-   The [xhb](https://hackage.haskell.org/package/xhb), along with some
    [support libaries](https://github.com/nickspinale/xhb-monad) I've written will replace
    the [X11](https://hackage.haskell.org/package/X11) library.

This project is in an early design stage, but may very well be in a working state by the summer of 2017.
