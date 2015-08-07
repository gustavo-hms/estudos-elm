module Markdown where

import String
-- import List
import Html exposing (Html, Attribute, div, text, input)
import Html.Attributes exposing (placeholder)
import Html.Events exposing (on, targetValue)
import StartApp

main = StartApp.start { model = modelo, view = exibir, update = atualizar }

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
        , div [] <| List.map exibirDocumento modelo
        ]


onInput : Signal.Address a -> (String -> a) -> Attribute
onInput endereço ação =
    on "input" targetValue (\nome -> Signal.message endereço (ação nome))


exibirDocumento : Documento -> Html
exibirDocumento doc =
    div [] [ text doc.nome ]


-- Atualização

atualizar : Ação -> List Documento -> List Documento
atualizar ação documentos =
    case ação of
        Filtrar nome -> filtrar modelo nome


filtrar : List Documento -> String -> List Documento
filtrar xs nome =
    if String.isEmpty nome
       then xs
       else List.filter (\x -> String.contains nome x.nome) xs


type Ação = Filtrar String
