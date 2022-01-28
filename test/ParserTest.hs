module ParserTest where
    import Text.Parsec (parse)
    import SmilieLang
    import Syntax
    import Eval

    t1 = ":-D"

    t2 = ":-):-D(( *tjoho* " -- if true, then *tjoho*

    t3 = ":-):-D(( :-D "

    t = parse expr "" -- test with 't str'