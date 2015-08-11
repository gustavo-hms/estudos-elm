module Documentos where

import String
-- import List
import Html exposing (Html, Attribute, div, text, input, br)
import Html.Attributes exposing (placeholder, style)
import Html.Events exposing (on, targetValue, onClick)
import Http
import Markdown
import Task exposing (Task)
import Signal exposing ((<~))
-- import StartApp

main : Signal Html
-- main = StartApp.start { model = modelo, view = exibir, update = atualizar }
main = Signal.foldp atualizar modeloInicial ações.signal |> Signal.map exibir


ações = Signal.mailbox <| Filtrar ""
markdown = Signal.mailbox {nome = "", uri = ""}


-- Modelo

type alias Modelo =
    { documentos : List (Documento {})
    , escolhido  : Maybe (Documento {})
    , markdown   : String
    }


type alias Documento a =
    { a |
      nome      : String
    , uri       : String
    }


modeloInicial : Modelo
modeloInicial =
    { documentos = documentos
    , escolhido  = Nothing
    , markdown   = ""
    }


documentos : List (Documento {})
documentos =
    [ { nome = "vim-duplicate"
      , uri  = "https://raw.githubusercontent.com/gustavo-hms/vim-duplicate/master/README.md"
      }
    , { nome = "shelter"
      , uri  = "https://raw.githubusercontent.com/rafaeljusto/shelter/master/README.md"
      }
    , { nome = "handy"
      , uri  = "https://raw.githubusercontent.com/trajber/handy/master/README.md"
      }
    , { nome = "rdap-client"
      , uri  = "https://raw.githubusercontent.com/registrobr/rdap-client/master/README.md"
      }
    ] 


-- Exibição

exibir : Modelo -> Html
exibir modelo =
    let escolhido = Maybe.withDefault { nome = "", uri = ""} modelo.escolhido 
        exibirDoc doc =
            exibirDocumento { doc | escolhido = escolhido.nome == doc.nome }
    in div []
           [ div [colunaEsquerda]
                 [ input
                    [ placeholder "Filtrar"
                    , onInput ações.address Filtrar
                    ]
                    []
                 , div [] 
                    (List.map exibirDoc modelo.documentos)
                 ]
           , div [colunaDireita] [Markdown.toHtml modelo.markdown]
           ]


onInput : Signal.Address a -> (String -> a) -> Attribute
onInput endereço ação =
    on "input" targetValue (\nome -> Signal.message endereço (ação nome))


exibirDocumento : Documento { escolhido : Bool } -> Html
exibirDocumento doc =
    div [ onClick markdown.address { doc - escolhido }
        , if doc.escolhido then docEscolhido else docNormal
        ]
        [ text doc.nome ]


colunaEsquerda : Html.Attribute
colunaEsquerda = style [("float", "left"), ("width", "20%"), ("margin", "2em")]


colunaDireita : Html.Attribute
colunaDireita = style [("float", "left"), ("width", "70%")]


estiloDoc = [("cursor", "pointer")]


docNormal : Html.Attribute
docNormal = style estiloDoc


docEscolhido : Html.Attribute
docEscolhido = style <| ("color", "blue") :: estiloDoc


-- Atualização

atualizar : Ação -> Modelo -> Modelo
atualizar ação modelo =
    case ação of
        Filtrar nome  ->
            { modelo | documentos <- filtrar nome modeloInicial.documentos }
        Escolher nome ->
            { modelo | escolhido <- escolher nome modelo.documentos }
        Exibir markdown ->
            { modelo | markdown <- markdown }


filtrar : String -> List (Documento {}) -> List (Documento {})
filtrar nome xs =
    if String.isEmpty nome
       then xs
       else List.filter (\x -> String.contains nome x.nome) xs


escolher : String -> List (Documento {}) -> Maybe (Documento {})
escolher nome documentos = List.head (filtrar nome documentos)


port requisições : Signal (Task Http.Error ())
port requisições = obterMarkdown <~ markdown.signal


obterMarkdown : Documento {} -> Task Http.Error ()
obterMarkdown doc =
    if doc.uri == ""
       then Task.succeed ()
       else Signal.send ações.address (Escolher doc.nome)
            `Task.andThen` \_ -> Http.getString doc.uri
            `Task.andThen` \mk -> Signal.send ações.address (Exibir mk)


type Ação
    = Filtrar String
    | Escolher String
    | Exibir String
