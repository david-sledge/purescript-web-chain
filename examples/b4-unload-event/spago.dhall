let config = ../../spago.dhall

in config // {
  sources = config.sources # [ "examples/b4-unload-event/src/**/*.purs" ],
  dependencies = config.dependencies
}
