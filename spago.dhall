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
{ name = "taihe"
, dependencies =
  [ "argonaut-codecs"
  , "arrays"
  , "control"
  , "data-default"
  , "datetime"
  , "effect"
  , "event"
  , "event-extra"
  , "filterable"
  , "foldable-traversable"
  , "foreign-generic"
  , "integers"
  , "math"
  , "maybe"
  , "newtype"
  , "numbers"
  , "prelude"
  , "profunctor-lenses"
  , "psci-support"
  , "unsafe-coerce"
  , "web-dom"
  , "web-events"
  , "web-file"
  , "web-html"
  , "web-touchevents"
  , "web-uievents"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
