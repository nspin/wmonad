# wmonad

wmonad is a tiling window manager based heavily on [xmonad](http://xmonad.org).
I have some very specific ideas about how I would like my windows to be managed.
Realizing this vision in a satisfying way within xmonad's extension model wouldn't be much easier than writing a new window manager.
wmonad is an attempt at the latter.

Many aspects of wmonad's design come directly from xmonad.
However, it differs in the following ways:

-   The `Stack` and `Layout` are replaced with a less flexible tree.
    Branches in this tree of windows can be stacked, or organized horizontally or vertically.
    Cursors marking branches allow the user to control groups of windows that are not focused, or even on a different screen.

-   The [xhb](https://hackage.haskell.org/package/xhb), along with some
    [support libaries](https://github.com/nspin/xhb-monad) I've written replace
    the [X11](https://hackage.haskell.org/package/X11) library.

More information about the xhb support libraries mentioned abouve can be found [here](http://nickspinale.com/articles/xhb-monad).
