{-
Welcome to a Spago project!
You can edit this file as you like.

Need help? See the following resources:
- Spago documentation: https://github.com/purescript/spago
- Dhall language tour: https://docs.dhall-lang.org/tutorials/Language-Tour.html

When creating a new Spago project, you can use
`spago init --no-comments` or `spago init -C`
to generate this file without the comments in this block.
-}
{ name = "web-chain"
, dependencies =
  [ "effect"
  , "exceptions"
  , "foldable-traversable"
  , "lists"
  , "maybe"
  , "prelude"
  , "psci-support"
  , "transformers"
  , "tuples"
  , "unsafe-coerce"
  , "web-dom"
  , "web-events"
  , "web-html"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
, license = "MIT"
, repository = "https://github.com/david-sledge/purescript-web-chain"
}
