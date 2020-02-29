module Convert exposing (..)


type Ingredient
    = Butter
    | Honey
    | Flour
    | Sugar
    | CasterSugar
    | Water


type Unit
    = Cups
    | Sticks
    | Grams
    | Ml


type alias Conversion =
    { ingredient : Ingredient
    , fromUnit : Unit
    , toUnit : Unit
    , rate : Float
    }


conversionRules : List Conversion
conversionRules =
    [ { ingredient = Butter, fromUnit = Cups, toUnit = Grams, rate = 226.8 }
    , { ingredient = Butter, fromUnit = Sticks, toUnit = Grams, rate = 113.4 }
    , { ingredient = Honey, fromUnit = Cups, toUnit = Grams, rate = 340.0 }
    , { ingredient = Flour, fromUnit = Cups, toUnit = Grams, rate = 125.0 }
    , { ingredient = Sugar, fromUnit = Cups, toUnit = Grams, rate = 200.0 }
    , { ingredient = CasterSugar, fromUnit = Cups, toUnit = Grams, rate = 225.0 }
    , { ingredient = Water, fromUnit = Cups, toUnit = Grams, rate = 236.59 }
    , { ingredient = Water, fromUnit = Cups, toUnit = Ml, rate = 236.59 }
    ]


convert : Conversion -> (Float -> Float -> Float) -> Float -> Float
convert c op amnt =
    op amnt c.rate


convertIngredient : Ingredient -> Unit -> Unit -> Float -> Float
convertIngredient ingredient fromUnit toUnit amount =
    let
        fromRule =
            conversionRules
                |> List.filter (\conversion -> conversion.ingredient == ingredient && conversion.fromUnit == fromUnit && conversion.toUnit == toUnit)
                |> List.head

        inverseRule =
            conversionRules
                |> List.filter (\conversion -> conversion.ingredient == ingredient && conversion.toUnit == fromUnit && conversion.fromUnit == toUnit)
                |> List.head
    in
    case fromRule of
        Just a ->
            convert a (*) amount

        Nothing ->
            case inverseRule of
                Just a ->
                    convert a (/) amount

                Nothing ->
                    amount
