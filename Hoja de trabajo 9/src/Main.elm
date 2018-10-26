module Main exposing (..)

import Browser
import Html exposing (Html)
import Html.Events exposing (onClick)


type alias Modelo = String


modeloInicial : Modelo
modeloInicial = ""

type alias Mensaje = String


refresh : Mensaje -> Modelo -> Modelo
refresh mensaje modelo = if mensaje == "C" then modeloInicial else  modelo ++ mensaje

suma n ns fs = 
    elemento (String.toInt (String.fromChar n ++ String.reverse (String.fromList ns))) + elemento (String.toInt (String.fromList fs))

mult n ns fs = 
    elemento (String.toInt (String.fromChar n ++ String.reverse (String.fromList ns))) * elemento (String.toInt (String.fromList fs))

elemento : Maybe Int -> Int
elemento h = case h of
    Just a -> a
    Nothing -> 0

resp1 jlist lista = case (jlist, lista) of
    (_, []) -> 0
    ([], n::ns) -> if n == '+' 
        then suma '0' [] ns else if n == '*' then mult '0' [] ns else resp1 (n::[]) (ns)
    (n::ns, f::fs) -> if f == '+'
        then suma n ns fs else if f == '*' then mult n ns fs else resp1 (n::f::ns) fs

respuesta modelo = resp1 [] (String.toList modelo)

cambio mode = Html.div [] [Html.text (mode)]

vista : Modelo -> Html Mensaje
vista modelo =  Html.div 
        []
        [
         cambio modelo,
        
        Html.div [] [
        Html.text (String.fromInt(respuesta  modelo)),
        Html.div [] [
        Html.button [onClick "1"] [Html.text "1"],
        Html.button [onClick "2"] [Html.text "2"],
        Html.button [onClick "3"] [Html.text "3"],
        Html.div [] [
        Html.button [onClick "4"] [Html.text "4"],
        Html.button [onClick "5"] [Html.text "5"],
        Html.button [onClick "6"] [Html.text "6"],
        Html.div [] [
        Html.button [onClick "7"] [Html.text "7"],
        Html.button [onClick "8"] [Html.text "8"],
        Html.button [onClick "9"] [Html.text "9"],
        Html.div [] [
        Html.button [onClick "0"] [Html.text "0"],
        Html.button [onClick "C" ] [Html.text "C"],
        Html.button [onClick "+" ] [Html.text "+"], 
        Html.div [] [
        Html.button [onClick "*"] [Html.text "*"] ] ] ] ] ] ] 
        ]


main = Browser.sandbox {
        init = modeloInicial,
        view = vista,
        update = refresh
    }