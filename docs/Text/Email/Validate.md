## Module Text.Email.Validate

#### `emailAddress`

``` purescript
emailAddress :: String -> Maybe EmailAddress
```

Smart constructor for an email address

#### `canonicalizeEmail`

``` purescript
canonicalizeEmail :: String -> Maybe String
```

#### `isValid`

``` purescript
isValid :: String -> Boolean
```

#### `validate`

``` purescript
validate :: String -> Either String EmailAddress
```

#### `runEmailParser`

``` purescript
runEmailParser :: forall e. String -> Either ParseError EmailAddress
```

Run a parser for an input string, returning either an error or a result.


