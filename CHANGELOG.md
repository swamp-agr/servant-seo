# Changelog for servant-seo

## 0.1.3 -- 2025-02-02

- Support GHC 9.4.x (see [#4](https://github.com/swamp-agr/servant-seo/pull/4)).
- Support newer `doctest` (see [#5](https://github.com/swamp-agr/servant-seo/pull/5)).

## 0.1.2 -- 2020-07-16

- Add missing instance for HTML with headers (auth context).
- Switch API extension from `serve` to `serveWithContext`.

## 0.1.1 -- 2020-07-12

- Fix frequency processing.
- Fix priority processing.

## 0.1.0 -- 2020-07-11

- /robots.txt deriving from API: Disallow, Sitemap, User-agent commands.
- /sitemap.xml deriving from API: Frequency, Priority optional tags included.
- sitemap index support.
