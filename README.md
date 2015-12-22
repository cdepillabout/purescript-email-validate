
purescript-email-validate
==================

[![Build
Status](https://travis-ci.org/cdepillabout/purescript-email-validate.svg)](https://travis-ci.org/cdepillabout/purescript-email-validate)

A small library providing an EmailAddress type.  Based on the Haskell library
[email-validate](https://hackage.haskell.org/package/email-validate).

- [Module documentation](docs/Text/Parsing/)

### Installing

    pulp dep install purescript-email-validate

### Building / Testing

    pulp dep update
    pulp build
    pulp test

### Usage

```
> import Text.Email.Validate (EmailAddress(), toString, emailAddress)
> :t emailAddress
String -> Either String EmailAddress
> emailAddress "test@email.com"
Right test@email.com
> emailAddress "bad@email@address"
Left "bad email address"
> :t toString
EmailAddress -> String
> toString $ emailAddress "test@email.com"
test@email.com
```
