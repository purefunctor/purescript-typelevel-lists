let conf = ./spago.dhall

in      conf
    //  { dependencies = conf.dependencies # [ "aff", "effect", "spec" ]
        , sources = conf.sources # [ "test/**/*.purs" ]
        }
