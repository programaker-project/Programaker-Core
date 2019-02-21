-record(template_entry, { id    :: binary()
                        , name  :: binary()
                        , owner :: binary() %% User id
                        , content :: [any()]
                        }).
