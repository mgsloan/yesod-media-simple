# yesod-media-simple

This package provides simple utilities for starting a web server which serves
some media. This makes serving media as easy as `serve image`. For example,
here's how you serve a simple diagram:

```haskell
import Diagrams.Prelude
import Yesod.Media.Simple

main = serveDiagram (circle 10 ||| square 20)
```

## Media Types

Currently `yesod-media-simple` supports serving the following media types:

* [Diagrams](http://projects.haskell.org/diagrams/), by using cairo to render to
  png. Just call `serveDiagram :: Diagram Cairo -> IO ()`, or the more generic
  `serve :: RenderContent a => a -> IO ()`.

* [JuicyPixels](https://hackage.haskell.org/package/JuicyPixels) images. Also
  includes utilities for putting JuicyPixels images in Diagrams and vice-versa.
