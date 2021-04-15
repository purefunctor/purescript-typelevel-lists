let conf = ../spago.dhall

in      conf
    //  { dependencies = conf.dependencies # [ "prelude", "psci-support" ]
        , sources = conf.sources # [ "examples/**/*.purs" ]
        }
