# servant-seo

## Installation

- Add `servant-seo` to project dependencies.

## Usage

1. Restrict API with `Disallow` combinator to prevent robots making requests.

  ```haskell
  type ProtectedAPI = Disallow "admin" :> Get '[HTML] AdminPage

  type StaticAPI = "cdn" :> Disallow "static" :> Raw
  ```

2. Add `Priority` and `Frequency` (optional step).

  ```haskell
  typw BlogAPI = "blog" :> Frequency 'Always :> Get '[HTML] BlogPage
  ```
  
3. Extend your API with something like: 

  ```haskell
  Warp.runSettings Warp.defaultSettings $ serveWithSeo website api server
  where
    website = "https://example.com"
  ```

You will have `/robots.txt` and `/sitemap.xml` automatically generated and ready to be served with your API.

See `Servant.Seo` module for detailed description and `example/Example.hs` for show case.

## Acknowledgements

This library would not have happened without these people. Thank you!

  - @fizruk and **Servant Contributors**: as source of inspiration.
  - @isovector: for `Thinking with types` book.
  
