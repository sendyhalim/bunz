# Bunz
JSON beautifier cli tool written in Haskell

## Usage
```
// Beautify plain json string
bunz "{\"foo\":\"bar\"}"
{
  "foo": "bar"
}

// Beautify the input
cat test.json | bunz
{
  "name": "sendy",
  "popular": false,
  "friend": {
    "name": "\"The rock\n\n\t?",
    "points": [
      1,
      2,
      34,
      5
    ]
  }
}
```

## Installation
Bunz requires [Haskell stack tool](https://github.com/commercialhaskell/stack) to install

```
stack install bunz
```
