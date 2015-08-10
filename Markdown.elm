module Markdown where

import String
-- import List
import Html exposing (Html, Attribute, div, text, input)
import Html.Attributes exposing (placeholder, style)
import Html.Events exposing (on, targetValue, onClick)
import StartApp

main : Signal Html
-- main = StartApp.start { model = modelo, view = exibir, update = atualizar }
main = Signal.foldp atualizar modelo mailbox.signal |> Signal.map (exibir mailbox.address)


mailbox = Signal.mailbox <| Filtrar ""


-- Modelo

type alias Documento =
    { nome      : String
    , uri       : String
    , escolhido : Bool
    }


modelo : List Documento
modelo =
    [ { nome = "vim-duplicate"
      , uri  = "https://raw.githubusercontent.com/gustavo-hms/vim-duplicate/master/README.md"
      , escolhido = False
      }
    , { nome = "shelter"
      , uri  = "https://raw.githubusercontent.com/rafaeljusto/shelter/master/README.md"
      , escolhido = False
      }
    , { nome = "handy"
      , uri  = "https://raw.githubusercontent.com/trajber/handy/master/README.md"
      , escolhido = False
      }
    , { nome = "rdap-client"
      , uri  = "https://raw.githubusercontent.com/registrobr/rdap-client/master/README.md"
      , escolhido = False
      }
    ] 


-- Exibição

exibir : Signal.Address Ação -> List Documento -> Html
exibir endereço modelo =
    div []
        [ input
            [ placeholder "Filtrar"
            , onInput endereço Filtrar
            ]
            []
        , div [] <| List.map (exibirDocumento endereço) modelo
        ]


onInput : Signal.Address a -> (String -> a) -> Attribute
onInput endereço ação =
    on "input" targetValue (\nome -> Signal.message endereço (ação nome))


exibirDocumento : Signal.Address Ação -> Documento -> Html
exibirDocumento endereço doc =
    div [ onClick endereço (Escolher doc.nome)
        , if doc.escolhido then docEscolhido else style []
        ]
        [ text doc.nome ]


docEscolhido : Html.Attribute
docEscolhido = style [("color", "blue")]


-- Atualização

atualizar : Ação -> List Documento -> List Documento
atualizar ação documentos =
    case ação of
        Filtrar nome  -> filtrar nome modelo 
        Escolher nome -> escolher nome documentos


filtrar : String -> List Documento -> List Documento
filtrar nome xs =
    if String.isEmpty nome
       then xs
       else List.filter (\x -> String.contains nome x.nome) xs


escolher : String -> List Documento -> List Documento
escolher nome xs =
    List.map (\x -> {x | escolhido <- x.nome == nome}) xs


type Ação
    = Filtrar String
    | Escolher String
