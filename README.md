
purescript-email-validate
==================

[![Build Status](https://travis-ci.org/cdepillabout/purescript-email-validate?branch=master)](https://travis-ci.org/cdepillabout/purescript-email-validate)

A small library providing an EmailAddress type.  Based on the Haskell library [email-validate](https://hackage.haskell.org/package/email-validate).

- [Module documentation](docs/Text/Parsing/)
- [Example usage](test/Main.purs)

### Installing

    pulp dep install purescript-email-validate

### Building / Testing

    pulp dep update
    pulp build
    pulp test

### Building with Docker

```
$ pushd docker/ && docker build --tag purescript-email-validate . && popd
$ docker run --rm --tty --interactive --volume `pwd`:/opt/src --workdir /opt/src --user `id -u`:`id -g` purescript-email-validate pulp dep install
$ docker run --rm --tty --interactive --volume `pwd`:/opt/src --workdir /opt/src --user `id -u`:`id -g` purescript-email-validate pulp build
$ docker run --rm --tty --interactive --volume `pwd`:/opt/src --workdir /opt/src --user `id -u`:`id -g` purescript-email-validate pulp test
```

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
