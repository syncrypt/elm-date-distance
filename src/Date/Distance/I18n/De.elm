module Date.Distance.I18n.De
    exposing
        ( LocaleConfig
        , locale
        )

{-| German locale.
@docs LocaleConfig
@docs locale
-}

import String
import Date.Distance.Types exposing (Locale, DistanceLocale(..))
import Date.Extra as Date exposing (Interval(..))


{-| Configure the localization function.
* `addSuffix` – turns `2 Tage` into `vor 2 Tagen` or `in 2 Tagen`
-}
type alias LocaleConfig =
    { addSuffix : Bool
    }


{-| Configure the German locale.

    locale = I18n.De.locale { addSuffix = True }
    inWords = { defaultConfig | locale = locale }
      |> inWordsWithConfig
-}
locale : LocaleConfig -> Locale
locale { addSuffix } order distance =
    let
        result =
            locale_ distance
    in
        if addSuffix then
            if order == LT then
                "in " ++ result
            else
                "vor " ++ result
        else
            result


locale_ : DistanceLocale -> String
locale_ distance =
    case distance of
        LessThanXSeconds i ->
            circa "weniger als" Second i

        HalfAMinute ->
            "einer halben Minute"

        LessThanXMinutes i ->
            circa "weniger als" Minute i

        XMinutes i ->
            exact Minute i

        AboutXHours i ->
            circa "ungefähr" Hour i

        XDays i ->
            exact Day i

        AboutXMonths i ->
            circa "ungefähr" Month i

        XMonths i ->
            exact Month i

        AboutXYears i ->
            circa "ungefähr" Year i

        OverXYears i ->
            circa "mehr als" Year i

        AlmostXYears i ->
            circa "beinahe" Year i


formatInterval : Interval -> String
formatInterval interval =
    case interval of
        Minute ->
            "Minute"

        Millisecond ->
            "Millisekunde"

        Second ->
            "Sekunde"

        Hour ->
            "Stunde"

        Day ->
            "Tag"

        Month ->
            "Monat"

        Year ->
            "Jahr"

        Quarter ->
            "Quartal"

        Week ->
            "Woche"

        Monday ->
            "Montag"

        Tuesday ->
            "Dienstag"

        Wednesday ->
            "Mittwoch"

        Thursday ->
            "Donnerstag"

        Friday ->
            "Freitag"

        Saturday ->
            "Samstag"

        Sunday ->
            "Sonntag"


pluralize s =
    if String.endsWith "e" s then
        s ++ "n"
    else
        s ++ "en"


singular : Interval -> String
singular interval =
    case interval of
        Minute ->
            "einer " ++ formatInterval interval

        _ ->
            "1 " ++ formatInterval interval


circa : String -> Interval -> Int -> String
circa prefix interval i =
    case i of
        1 ->
            prefix ++ " " ++ singular interval

        _ ->
            prefix ++ " " ++ toString i ++ " " ++ (pluralize <| formatInterval interval)


exact : Interval -> Int -> String
exact interval i =
    case i of
        1 ->
            "1 " ++ formatInterval interval

        _ ->
            toString i ++ " " ++ formatInterval interval ++ "en"
