{ name = "halogen-project"
, dependencies =
  [ "aff"
  , "arrays"
  , "console"
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
