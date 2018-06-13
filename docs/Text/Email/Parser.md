## Module Text.Email.Parser

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

#### `EmailParser`

``` purescript
type EmailParser a = Parser a
```

#### `addrSpec`

``` purescript
addrSpec :: EmailParser EmailAddress
```

A parser for email addresses.

#### `domainPart`

``` purescript
domainPart :: EmailAddress -> String
```

#### `localPart`

``` purescript
localPart :: EmailAddress -> String
```

#### `toString`

``` purescript
toString :: EmailAddress -> String
```

Converts an email address to a 'String'


