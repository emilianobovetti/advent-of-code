platform roc/core
    requires {}{ main : Task {} [] } # TODO FIXME
    exposes []
    packages {}
    imports [ Task.{ Task } ]
    provides [ mainForHost ]
    effects fx.Effect
        {
            openFile : Str -> Effect U64,
            closeFile : U64 -> Effect {},
            getFileBytes : U64 -> Effect (List U8),
            getFileLine : U64 -> Effect Str,
            putLine : Str -> Effect {},
            getLine : Effect Str
        }

mainForHost : Task {} [] as Fx
mainForHost = main
