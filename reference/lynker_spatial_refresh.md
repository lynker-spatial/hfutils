# Refresh an existing Lynker Spatial token

Refresh an existing Lynker Spatial token

## Usage

``` r
lynker_spatial_refresh(token, ..., client = lynker_spatial_client())
```

## Arguments

- token:

  An existing httr2 OAuth2 token to refresh.

- ...:

  Unused; reserved for future options.

- client:

  An OIDC client, as returned by \[lynker_spatial_client()\].

## Value

A refreshed httr2 OAuth2 token.
