module Flappy_Circle exposing (..)

import Html as Html
import Html.Attributes as HtmlAtr
import Html.Events exposing (onClick)

import Platform.Cmd as Cmd
import Platform.Sub as Sub
import Mouse as Mouse
import Keyboard as Key 
import Svg exposing (..)
import Svg.Attributes exposing (..)
import AnimationFrame as Anim
import Random exposing (..)


{-
svgStyle = HtmlAtr.style [("width","500px"),("height","600px"),
                          ("margin","200px"),("margin-top","0px"),("float","left"),
                          ("margin-bottom","10px")]
 -}

svgStyle  = HtmlAtr.style  [("width","500px"),("height","600px"),("background-image","url('background.png')"),
                            ("margin","200px"),("margin-top","0px"),("float","left"),
                            ("margin-bottom","10px")]  

page0Style = HtmlAtr.style [("margin-top","130px"),("text-align","center"),("margin-left","22%"),
                            ("font-family","Comic Sans MS"),("position","absolute"),
                            ("color","#5c5c8a"),("font-size","18px")]

page2Style = HtmlAtr.style [("margin-top","130px"),("text-align","center"),("margin-left","35%"),
                            ("font-family","Comic Sans MS"),("position","absolute"),
                            ("color","#5c5c8a"),("font-size","20px")]


titleStyle = HtmlAtr.style [("font-size","70px"),("color","#7094db")]  

scoreStyle = HtmlAtr.style [("height","200px"),("padding-top","100px"),
                            ("font-family","Comic Sans MS"),("font-size","28px")]

buttonStyle = HtmlAtr.style [("font-size","20px"),("padding","4px"),("margin","5px"),
                              ("border-radius","7px"),("background-color","#eff5f5"),
                              ("font-family","Comic Sans MS")] 


type alias Model = 
        {   counter : Int,
            position : {x:Int, y:Int},
            stage : Int,
            pipe : {p:Int, q:Int},
            pipX : Int,
            time : Int

        }
--Time = float       
type Msg = Begin | KeyMsg Key.KeyCode |Tick Float | RandResult (Int,Int) | TryAgain

init  = ({counter = 0, position = {x=200,y=300},stage = 0, pipX = 600,pipe={p=100,q=300},time=0},Cmd.none)

subscriptions model = Sub.batch 
                        [Anim.times Tick, Key.downs KeyMsg]

update msg model =  case model.stage of

       0 -> case msg of 
                Begin -> ({model|stage = 1},Cmd.none)
                _   -> (model, Cmd.none)

       1 -> case msg of
            KeyMsg 32 -> let 
               posY = model.position.y

             in (
                 {model| position = { x = model.position.x, y = model.position.y-60}} ,
                 Cmd.none
                )

            Tick t -> 

                  let
                    posX = model.position.x
                    posY = model.position.y + 3
                    modelN = {
                                model| position = {x=posX,y=posY},
                                       stage = if (posY > 15) && (posY < 585)
                                                 then if ((posX-model.pipX)>= -5 && (posX-model.pipX)<=33) && ((posY < model.pipe.q+18) || (posY > (model.pipe.q + model.pipe.p-10)))
                                                         then 2
                                                         else 1
                                                 else 2,

                                       counter =  if posX == (model.pipX + 35)
                                                      then model.counter + 1
                                                      else model.counter,
                                      
                                      

                                       pipX = model.pipX - 3,
                                       time = model.time + 1

                             }
                    cmdR = generate RandResult(pair (int 90 150) (int 20 400))  --generate a pair of random numbers

                   in (modelN,cmdR)

                
            RandResult (dis, strP) -> if (model.time % 210) == 0
                                        then ({model | pipX = 600,pipe = {p=dis,q=strP}},Cmd.none)
                                        else (model,Cmd.none)
                                           

            _  -> (model,Cmd.none)



       _  -> case msg of   --stage = 2

            TryAgain -> ({model | counter = 0, position = {x=200,y=300},stage=0, pipe={p=200,q=100},pipX = 600,time=0} , Cmd.none)
            _   -> (model,Cmd.none)
         
    


page0:Model->Html.Html Msg
page0 model = Html.div [page0Style]
                 [
                    Html.h1 [titleStyle] [text "Flappy Circle"],
                    Html.br [] [],
                    Html.div [] [ text "OH NOoo! your circle got into a dangerous pipe area.",
                                 Html.br [] [],
                                 text  "Press the spacebar to jump your circle and fly as far as you can without hitting a pipe.",
                                 Html.br [] [],
                                 text "Also be careful not to fly over the border."

                                ],
                    Html.br [] [],
                    Html.div [] 
                           [
                              Html.button [onClick Begin, buttonStyle] [text "Begin"]
                           ]
                 ]


page1:Model->Html.Html Msg
page1 model = let
     posX =  toString model.position.x
     posY =  toString model.position.y
     pipeX = toString model.pipX
     h1 = toString model.pipe.q
     y1 = toString (model.pipe.q + model.pipe.p)
     h2 = toString (600 - model.pipe.q - model.pipe.p)


       in Html.div[]
         [

            Html.div [svgStyle] 

                 [
                    svg [width "500",height "600"]
                        [        --center x
                           circle [
                                     cx posX,cy posY, r "15", fill "#ff9900",
                                     stroke "#cc7a00", strokeWidth "2"
                                  ] [],
                           rect
                                  [  x pipeX, y "0", width "30px",height h1,
                                     fill "#88cc00", stroke "#511515", strokeWidth "2"
                                  ] [],
                           rect  [
                                     x pipeX, y y1, width "30px",height h2, 
                                     fill "#88cc00", stroke "#511515", strokeWidth "2"
                                  ] []
                        ]
                   
                 ],

            
            Html.div [scoreStyle] [text (toString model.counter)]
            

         ]     
           
            

page2:Model->Html.Html Msg
page2 model = Html.div [page2Style] 
                    [
                      Html.h1 [titleStyle] [text "Game over"],
                      Html.text "Your score: ",
                      Html.h2 [] [text (toString model.counter)],
                      
                      Html.button [onClick TryAgain, buttonStyle] [text "Try Again"]

                      
                    ]

view:Model->Html.Html Msg
view model = case model.stage of
     0 -> page0 model             --welcone page
     1 -> page1 model             --game page
     _ -> page2 model             --game over page
            

     


    


main = Html.program
    {

    init = init,
    update = update,
    view = view,
    subscriptions = subscriptions

   }
