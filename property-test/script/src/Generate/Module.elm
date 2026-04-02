module Generate.Module exposing (generator)

{-| Generate a complete random Elm module source string.
-}

import Generate.Declaration as Declaration
import Random exposing (Generator)


{-| Generate a random Elm module with the given module name.
Uses depth to control expression complexity.
-}
generator : String -> Int -> Generator String
generator moduleName depth =
    Random.int 1 5
        |> Random.andThen (\n -> randomList n (Declaration.generator depth))
        |> Random.map
            (\decls ->
                "module " ++ moduleName ++ " exposing (..)\n\n\n" ++ String.join "\n\n\n" decls ++ "\n"
            )



-- HELPERS


randomList : Int -> Generator a -> Generator (List a)
randomList n gen =
    if n <= 0 then
        Random.constant []

    else
        Random.map2 (::) gen (randomList (n - 1) gen)
