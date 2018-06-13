## Module Text.Email.Validate

#### `isValid`

``` purescript
isValid :: String -> Boolean
```

Validates whether a particular string is an email address

#### `validate`

``` purescript
validate :: String -> Either String EmailAddress
```

If you want to find out *why* a particular string is not

#### `emailAddress`

``` purescript
emailAddress :: String -> Maybe EmailAddress
```

Smart constructor for an email address

#### `canonicalizeEmail`

``` purescript
canonicalizeEmail :: String -> Maybe String
```

Checks that an email is valid and returns a version of it

#### `runEmailParser`

``` purescript
runEmailParser :: String -> Either ParseError EmailAddress
```

Run a parser for an input string, returning either an error or a result.


### Re-exported from Text.Email.Parser:

#### `EmailAddress`

``` purescript
newtype EmailAddress
  = EmailAddress { localPart :: String, domainPart :: String }
```

Represents an email address.

##### Instances
``` purescript
Generic EmailAddress _
Show EmailAddress
Eq EmailAddress
```

#### `toString`

``` purescript
toString :: EmailAddress -> String
```

Converts an email address to a 'String'

#### `localPart`

``` purescript
localPart :: EmailAddress -> String
```

#### `domainPart`

``` purescript
domainPart :: EmailAddress -> String
```

#### `addrSpec`

``` purescript
addrSpec :: EmailParser EmailAddress
```

A parser for email addresses.

