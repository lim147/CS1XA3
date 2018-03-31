module Meijing_CV exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)


outsideStyle = style [("background-color","#f2f2f2"),("width","100%"),
                      ("height","1930px")]

blockStyle = style [("background-color","#f2f2f2"),("width","100%"),
                      ("height","20px")]


mainStyle = style [("margin","10%"),("margin-top","3%"),("margin-bottom","3%"),
                   ("background-color","#ffffff")]
                   
                   

leftStyle = style [("float","left"),("width","180px"),("padding","15px")]               

imgStyle = style [("height","160px"),("weight","160px"),
                  ("padding","20px"),("padding-bottom","0px"),("padding-top","25px")]

sidebar = style [("width","70"), ("position","relative"),("bottom","7px")]

nameStyle = style [("color","#32C79E"),("text-align","right"),
                   ("font-family","Verdana"),
                   ("font-size","28px")]

inforStyle = style [("color","#666D6B"),("text-align","right"),("font-family","Trebuchet MS"),
                    ("text-indent","10px"),("line-height","0.5"),("width","180px")]

githublogoStyle = style [("height","50px"),("weight","50px"),
                         ("float","left"),("padding-left","10px")]
inlogostyle = style [("height","60px"),("weight","60px"),
                     ("margin-top","-6px")]

rightStyle = style[("font-family","Trebuchet MS"),("font-size","15px"),
                   ("margin-left","220px"),("padding-left","25px"),
                   ("border-left", "2px solid #cccccc"),
                   ("padding-right","5px"),("padding-top","25px"),
                   ("padding-bottom","60px")]
                   


headerStyle = style [("color","#384745")]

paragrapgStyle = style [("color","#667270"),("line-height","1.5")]


footerStyle = style [("text-align","center"),("font-family","Trebuchet MS"),
                     ("font-size","15px"),("color","#667270")]

                   
main: Html msg
main = div [outsideStyle]
  [
    div [blockStyle] [],
    div [mainStyle]
      [

       section [leftStyle]
         [
           img [src "my_photo.jpg",imgStyle ] [],
           div [sidebar] [],
           div [] 
               [
                 h3 [nameStyle] [text "Meijing Li"]

                ],
            div [sidebar] [],
            div [inforStyle]
                [
                 p [] [text "tel"],
                 p [] [text "email"],
                 p [] [text "1280 Main Street West"],
                 p [] [text "Hamilton,ON"],
                 p [] [text "L8S 4M6"],

                 a [href "https://github.com/lim147/CS1XA3",target "_blank"] 
                     [img [src "github.jpg",githublogoStyle] []],

                 a [href "https://www.linkedin.com/in/meijing-li/",target "_blank"] 
                     [img [src "linkedin.jpg",inlogostyle] []]
                ]
            
          ],


        section [rightStyle] 
          [
             div []
               [
                 h2 [headerStyle] [text "SUMMARY"],
                 p [paragrapgStyle] 
                      [ text "Currently in my first year at ",
                        strong [] [text "McMaster University "],
                        text "specializing in ",
                        strong [] [text "Computer Science"],
                        text ".I am a detail-oriented student with strong academic skills and the ability to learn concepts quickly. With the interest in the software code design, I am looking to apply my education and experience to a job in the application development field."
                      ]
                ],

            br [] [],

            div []
              [

                 h2 [headerStyle] [text "EDUCATION"],
                 p [paragrapgStyle] 
                    [
                      div [style [("line-height","0.3")]] 
                              [

                                h4 [headerStyle] [text "Bachelor of Computer Science I (CO-OP)"],
                                p [] [text "McMaster University,ON,Canada"]
                              ],

                     p  [style [("line-height","1.5")]]
                            [text "Related coursework:",
                             ul []
                               [
                                li [] [text "Introduction to Computational Thinking (Haskell)"],
                                li [] [text "Introduction to Programming (Python)"],
                                li [] [text "Cs Practice & Experience (Bash Script, Elm, Git)"]
                               ],

                             a [href "transcript.pdf" , target "_blank"]
                               [text "Transcript"]
                            ]

        
                    
                    ]

              ],

            br [] [],

            div [] 
             [
               h2 [headerStyle] [text "EXPERIENCE"],
               p [paragrapgStyle]
                    [
                      
                    h4 [headerStyle] [text "Academic:"],
                    p []
                         [
                            li [] [text "Tutor for middle school students with Excel project."],
                            li [] [text "Worked as part of a team to achieve a math model design for solving Optimal Routing Problem."],
                            li [] [text "Conducted analysis to address consumer reduction problem by analyzing customer feedbacks base on the big data knowledge."]
                         ],
                    h4 [headerStyle] [text "Volunteer:"],
                    p []
                         [
                            li [] [text "Volunteered in Red Cross to raise fund for the disabled"]

                         ]

                    ]

             ],

            br [] [],

            div []
              [
                h2 [headerStyle] [text "PROJECT"],
                p [paragrapgStyle]
                     [
                        h4 [headerStyle] [text "Programming homework:"],
                        p []
                            [
                                li []
                                    [
                                        b []  [text "Web App"],
                                        text " -- built by using Elm Architecture Functionality" 
                                    ],
                                ul []
                                     [
                                        text "⚪ " ,
                                        a [href "flappy_circle.html" , target "_blank"] [text "Flappy Circle "],
                                        text "- A simple game, almost same with Flappy Bird. Control the circle by spacebar and fly through as many pipes as you can to get higher marks.",
                                        br [] [],
                                        br [] [],
                                        text "🌸 " ,
                                        a [href "drawing_flower.html" , target "_blank"] [text "Drawing a Flower "],
                                        text "- A simple animation to visualize a math equation. The path of the moving circle is of flower shape, based on the equation ",
                                        i [] [text "r = 1 + 3*sin(2*θ)^2 ."],
                                        text "Try interact with the circle by clicking the mouse or button."
                                     ]     
                                      

                                    


                            ]


                     ]

              ],

            br [] [],

            div []
              [
                
                h2 [headerStyle] [text "ACTIVITIES AND HONORS"],
                p [paragrapgStyle]
                     [
                       h4 [headerStyle] [text "Activities:"],
                       p []
                            [
                               li [] [text "Member of Senior Engineering Technical Club"],
                               li [] [text "Member of high school volunteer club"], 
                               li [] [text "Leader of the math model building group"]
                            ],
                        h4 [headerStyle] [text "Honors:"],
                        p []
                            [
                              li [] [text "Won second price of  Mathematical Olympiad | 2016"]
                            ]


                     ]
              ],
              
              br [] [],
              div []
                     [
                        h2 [headerStyle] [text "SKILLS"]

                     ],
    
            
            
              div [style [("float","left")]]
                [
                   h4 [headerStyle] [text "Programming:"],
                   p [paragrapgStyle]
                        [
                          li [] [text "Experienced in Python"],
                          li [] [text "Proficient in Haskell"],
                          li [] [text "Familiar with Bash"],
                          li [] [text "Familiar with Git"],
                          li [] [text "Familiar with Elm"],
                          li [] [text "Familiar with HTML & CSS"],
                          li [] [text "Knowledge on C++"]

                        ]

                ],


                div [style [("float","left"),("padding-left","20px"), ("padding-right","20px")]]
                [
                   h4 [headerStyle] [text "Software:"],
                   p [paragrapgStyle]
                        [
                          li [] [text "Proficient in MS office",
                                 br [] [],
                                 div [style [("text-indent","20px")]] [text "including Word, Excel, PowerPoint"]
                                 ],

                          li [] [text "Proficient in Jupyterhub"],
                          li [] [text "Proficient in MapleSim"],
                          li [] [text "Familiar with Photoshop"],
                          li [] [text "Familiar with Matlab"]
                          

                        ]
                    ],


                 div [style [("padding-top","0.5px"),("padding-bottom","90px")]]
                [

                   h4 [headerStyle] [text "Laboratory and Safety Training:"],
                   p [paragrapgStyle]
                        [
                          li [] [text "WHIMIS trained"],
                          li [] [text "Standard First Aid"]
                        

                        ]

                ]

             


                 
          ]
       

      ],
      footer [footerStyle] [text "Designed by Meijing Li @ 2018"]
  ]


