## Module Text.Email.Parser

#### `EmailAddress`

``` purescript
newtype EmailAddress
```

Represents an email address.

##### Instances
``` purescript
instance genericEmailAddress :: Generic EmailAddress
instance showEmailAddress :: Show EmailAddress
instance eqEmailAddress :: Eq EmailAddress
```

#### `localPart`

``` purescript
localPart :: EmailAddress -> String
```

#### `domainPart`

``` purescript
domainPart :: EmailAddress -> String
```

#### `toString`

``` purescript
toString :: EmailAddress -> String
```

Converts an email address to a 'String'

#### `addrSpec`

``` purescript
addrSpec :: EmailParser EmailAddress
```

A parser for email addresses.


