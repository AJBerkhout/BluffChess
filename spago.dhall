{ name = "halogen-project"
, dependencies =
  [ "aff"
  , "argonaut-codecs"
  , "argonaut-generic"
  , "arrays"
  , "console"
  , "const"
  , "css"
  , "effect"
  , "foldable-traversable"
  , "halogen"
  , "halogen-css"
  , "maybe"
  , "prelude"
  , "spec"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
