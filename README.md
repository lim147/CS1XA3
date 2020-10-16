
# Software Requirements Specification
## For The G-ScalE Mini-Game Battery Project

Version 0.1  
Prepared by Team Ludus  
[Alice Ip](@ipa1) - 400078727  
[Jack Buckley](@bucklj4) - 	400144747  
[Meijing Li](@lim147) - 400110713  
[Shuo He](@hes11) - 400023520  
[Yunfei Yang](@yangy113) - 400049426  
Instructor: [Dr. Jacques Carette](@carette)  
Course: [COMPSCI 4ZP6: Capstone Project](https://www.cas.mcmaster.ca/~carette/CS4ZP6/2020_21/index.html)  
Date: 2020-10-10   

Table of Contents
=================
[[_TOC_]]

## Revision History
| Name | Date    | Reason For Changes  | Version   |  
| ---- | ------- | ------------------- | --------- |  
| ALL  | 10-18-2020 | First Release | Version 1.0  |  
|      |         |                     |           |  
|      |         |                     |           |  

## 1. Introduction

### 1.1 Document Purpose
##### 1.1.a Background 
The G-ScalE Mini-Game Battery Project is a capstone project for COMPSCI 4ZP6 at McMaster University. This project is developed by Team Ludus, a team that consists of five fourth-year Computer Science students from McMaster University under the supervision of [Sasha Soraine](@sorainsm). Completion of this capstone project is a mandatory requirement for graduation and also a process for students to apply their knowledge in a year long project.

Our capstone group is developing the G-ScalE Mini-Game Battery Project, a project composed of five to ten single-player mini games designed in Unity, to measure approximately five out of 46 identified human cognitive and psychomotor skills. Each game will take players no more than a minute to play, and will be used to establish player competencies in a specific cognitive or psychomotor group. The structure of the games will be similar to minigame compilations like Mario Party.

##### 1.1.b Goals
The purpose of the capstone project is to give students an opportunity to apply and practice their knowledge learned through their university study experience to create a set of mini-games that could practice their creativity, teamwork, and technical skills. This will also let students participate in a full life-cycle of software development, which enhances the understanding of the software design process. 

The goal of the G-ScalE Mini-Game Battery Project is to measure a subset of cognitive and motor abilities through the process of playing and enjoying the games.

##### 1.1.c Purpose
The purpose of this requirements document is to lay out who the stakeholders of this project are, the constraints and scope of the project, as well as the functional and non-functional requirements that need to be met to ensure the success of the project. In this way, this document serves as a contract between us, Team Ludus, and our stakeholders on what we will deliver to ensure our solution fully addresses the problem our stakeholders face, as laid out in our previous project summary document, all while ensuring we meet the many individual requirements that compose this problem.

### 1.2 Stakeholders
#### The Client
* [Dr. Jacques Carette](@carette): The professor for the Computer Science Capstone Project: COMPSCI 4ZP6. He establishes the requirements, milestones, deadlines, and other course-related objectives. He is also responsible for evaluating and grading the final project.

* [Sasha Soraine](@sorainsm): A PhD. Candidate in the Department of Computing and Software at McMaster University. She is our supervisor for the G-ScalE Mini-Game Battery Project. She will provide us with detailed information and requirements about the project and assist us in finishing the challenges during the development process.


* [Ethan Chan](@chaneh) and [Brendan Fallon](@fallonbr): These are the TAs for COMPSCI 4ZP6. They will be responsible for grading and providing feedback on project deliverables.


#### The User Group
* Players: The people who will be playing the mini-games, consisting of people of different ages. They will participate in gameplay designed to provide data that we will measure to extract cognitive and motor ability aptitudes.

* Other students taking COMPSCI 4ZP6: The other students taking the capstone course will submit feedback and bug reports during the testing phase of the project.

#### Others
* Team Ludus: The developers of the G-ScalE Mini-Game Battery Project. This team consists of five students enrolled in the Computer Science Capstone Project: COMPSCI 4ZP6. The final deliverable and the documents will be made by these team members who will also give a demo presentation of the project at the end of the semester to a panel of judges at the Faculty of Engineering Capstone Day.

* Team Mactivision: The other team working under our supervisor to develop a different set of mini-games for the G-ScalE Mini-Game Battery Project. The members of this group include: [Bryan Chiu](@chiub1), [Sijie Zhou](@zhous16), [Kunyuan Cao](@caok10), [David Hospital](@hospitad), and [Mike Tee](@teemh). We will be collaborating with this group at a further stage of development in the project.

* Panel of judges: The judges will critique the final product during showcase at the Faculty of Engineering Capstone Day in early April. The panel consists of people with different backgrounds in computer science and game design.

### 1.3 Mandated Constraints

##### Solution Constraints
* The game must be developed using the Unity game development software.
* The game must be modular: a change of controller must not change the underlying test.
* Each mini-game should take players no more than one minute to finish.

##### Implementation Environment
* Computer System: The mini-games must be designed to be operating system independent.

* Input devices: The primary mode of input is the keyboard, and games must first be designed with this in mind.

* Output devices: The output device will be the computer monitor and speakers.

##### Partner or Collaborative Applications
* GitLab: Our ability to track issues and manage project progress will be limited to what is supported by GitLab.

* Microsoft Teams: Due to COVID-19, our team members are unable to meet in person, so our meeting collaboration has been limited to what is supported by Microsoft Teams.


##### Off-the-Shelf Software
* Unity: Game functionality will be limited to what is supported by the Unity platform.


##### Schedule Constraints
* All course deliverables must be completed on time according to the schedule provided by [Dr. Carette](@carette). The final project must be fully finished by April 5th.

##### Budget Constraints
* There are no budget constraints for this project. For the purposes of our project, we will not be paid nor will we need to pay for any software.

##### Final Product Constraints
* The final product must consist of several one-minute mini-games that will measure at least five cognitive and motor abilities from the set of identified cognitive and motor abilities.

### 1.4 Work Scope
##### Existing Inspirations
* [Super Mario Party](https://www.nintendo.com/games/detail/super-mario-party-switch/) (Nintendo, 2018):  A party video game where players compete with each other while having fun by playing several small games to earn points. Unique challenges are designed in those small games that require different abilities to be successful. Games in the G-ScalE Mini-Game Battery Project will be designed similarly to the mini-games found in Super Mario Party.
* [Cyclone](https://www.icegame.com/category/93/cyclone) (ICE, 1982): Arcade game designed for 1-3 players consisting of a circle of light up rings and lightbulbs. The rings and lightbulbs light up in some designated order, and the player must try to press a button when their desired ring or lightbulb lights up. Depending on the position of the light when the player presses the button, the player will get a score. Different positions give different points. This game tests the time to contact ability, as the player must watch as the lights approach their desired position, and press it when it reaches it.

##### Context of the Work
* The completion of this project is the focus of the course COMPSCI 4ZP6 and is a key requirement needed to graduate from the undergraduate computer science program at McMaster. 
* Beyond the end of the course, future work on the project could entail the development of further mini-games testing other abilities as well as the use of other input controllers apart from the keyboard.

### 1.5 Product Scope
##### Product Boundary
* The final product will be a series of mini-games consisting of at least five selected cognitive and motor abilities to be measured.
* The product will not be commercially sold and the platform will solely be laptop and desktop computers.
* Each mini-game is independent and has no continuity; players can choose which mini-game to start from freely.

##### Mini-Game Set
The G-ScalE Mini-Game Battery Project is a set of five games aimed to test the following abilities: object recognition, token change direction, time to contact, audial perception, and selective/focused visual. The game descriptions are as below:

###### Flip-Flop-Clear
A 2-D card game where 9 cards are placed face down on the screen in a 3 x 3 array. Each card has a different image, and three of the images on the cards are of the same type. For example, you could have a red apple card, a green apple card, and an apple tree card. When the player clicks on a card, the card flips over and the player can see its image. When the second card is turned over, if its image is of the same type as the first card, both cards remain face up, otherwise both cards go face down. The third card is flipped over in the same way. When a player has three cards' images of the same type all face up, this type of cards is cleared. To win the game, the player must clear all three types of cards. The object recognition ability is tested through the game as a player must recognize the object images on the cards and identify the objects of the same kind in a short amount of time.

###### Holy Sinkholes!
In this 2-D game, the user must dodge impending sinkholes that appear in the ground below them by carefully (and quickly!) moving left or right. The user will be given an indication that a sinkhole is about to appear, but this indication will be short, so they will have to choose the direction they move towards very quickly. Moreover, if the user moves all the way to one side of the screen, they will appear on the opposite side, so it is not possible to seek shelter and the player is always on the move. As an added gameplay challenge, once the user has been successful in dodging the sinkholes for long enough, garbage asteroids will begin falling vertically from the top of the screen. The user must then dodge both the the sinkholes below them *and* the asteroids coming from above them by moving in the correct direction quickly. Token change direction is tested through this game by seeing if the user chooses the correct direction to move towards and how quickly they can decide to change their player's direction in order to meet the objective of the game.

###### Green Light Go
In this game, which is inspired by the arcade game Cyclone, the user is presented with a top-down view of a circular game area. Along the perimeter of this circle, there are numerous light bulbs spaced evenly apart. One of these light bulbs is designated as the target. When the game begins, the light bulbs light up sequentially such that only one light bulb is lit at any given moment in time. The goal of the game is to have the user press a key on the keyboard to stop the light when it hits the target light bulb. The score that the user receives is directly related to how close the light bulb the light stopped at is to the target bulb. As an added gameplay challenge, the speed that the bulbs light up can get quicker. This game tests the time-to-contact of the player as it measures the user's hand-eye coordination and how quickly the user can press a button when indicated to do so.

###### Pianist
A music game where each key represents a musical note and the player is asked to listen to a section of music and repeat the tune through pressing the keyboard. The music will be slow and simple for the first several rounds; as the gameplay challenge increases, it will become faster and more complex. More complex tunes will consist of more notes and notes which are closer in pitch. The final score will depend on the correct number of musical notes and the correct order of these notes between the expected tune and player's inputs. This game will test the ability of audial perception and measures how accurately they can repeat the music by remembering the melody.


###### Cup Swap
An object will be displayed to the player, and then it will be put under a cup. There are two other identical cups, but with nothing under it. The three cups will swap positions for a short duration, then the player must choose the cup that they think has the object under it. The selective visual ability is tested as the player tries to keep track of the location of the cup containing the object. The speed of the swapping will increase and more cups will be introduced in later rounds.


##### Product Use Case (PUC) Table  
| PUC | PUC Name | Actor(s) | Input/Output |  
| ---- | ------- | ----------| --------- | 
| <a name="puc-1"></a>1 | Choose Game | Player | Key Input(In), Initial Game Data (Out)  |  
| <a name="puc-2"></a>2 | Exit Game | Player | Key Input(In), VOID |  
| <a name="puc-3"></a>3 | Pause Game | Player | Key Input(In), Game Status(Out) |  
| <a name="puc-4"></a>4 | Resume Game | Player | Key Input(In), Game Status(Out) |  
| <a name="puc-5"></a>5 | End Game | Player | Key Input(In), Game Status(Out) |   
| <a name="puc-6"></a>6 | Check Final Score | Player | Key Input(In), Score Data(Out) |  
| <a name="puc-7"></a>7 | Check Ability Report | Player |Key Input(In), Player Game Data (Out) |  
| <a name="puc-8"></a>8 | Move Character | Player | Key Input(In), Character Position(Out) |  
| <a name="puc-9"></a>9 | Get Hit | Player | Key Input(In), Player Game Data(Out) |  
| <a name="puc-10"></a>10 | Play the Piano | Player | Key Input(In), Player Game Data(Out), Auditory Data(Out) |   
| <a name="puc-11"></a>11 | Select the Cup | Player | Key Input(In), Player Game Data(Out) |  
| <a name="puc-12"></a>12 | Select the Green Light | Player | Key Input(In), Player Game Data(Out) |  
| <a name="puc-13"></a>13 | Show Game Instructions | Player | Key Input(In), Game Status(Out) |  
| <a name="puc-14"></a>14 | Dismiss Game Instructions | Player | Key Input(In), Game Status(Out) |  
| <a name="puc-15"></a>15 | Flip First Card | Player | Key Input(In), Game Status(Out) |  
| <a name="puc-16"></a>16 | Flip Second Card | Player | Key Input(In), Game Status(Out) |  
| <a name="puc-17"></a>17 | Flip Third Card | Player | Key Input(In), Game Status(Out) |  
| <a name="puc-18"></a>18 | Flip-Flop Clear | Player | Key Input(In), Game Status(Out) |  
| <a name="puc-19"></a>19 | Check Auto End | Player | Key Input(In), Game Status(Out) |  
| <a name="puc-20"></a>20 | Change Settings | Player | Key Input(In), Game Status(Out) |  
| <a name="puc-21"></a>21 | Change Volume | Player | Key Input(In), Game Status(Out) |  
| <a name="puc-22"></a>22 | Close Settings | Player | Key Input(In), Game Status(Out) |  
| <a name="puc-23"></a>23 | Exit End Screen | Player | Key Input(In), Game Status(Out) |



##### States Table
The following lists the possible states in the game, and a brief description of the purpose of the state.

| State Name        | Description of State |
| ----------------- | ---------------------------------- |
| IN_MAIN_MENU      | Main menu is shown                                                                  |
| PAUSE(*g*)        | Game *g* is the previously active game, and is now in the pause menu.               |
| SETTINGS(*g*)     | Game *g* is the previously active game, and is now in the settings menu.            |
| ACTIVE(*g*)       | Game *g* is the current game being played.                                          |
| FINISHED(*g*)     | Game *g* has ended.                                                                 |
| INSTRUCTIONS(*g*) | Game *g* was chosen from the main menu, and now shows the instructions for game *g* |


##### Individual Product Use Cases
| [PUC No.1](#puc-1) | Event: Choose Game |  
| ---- | ------- | 
| Trigger | Player selects a mini-game from the game menu. |  
| Preconditions | Game state is IN_MAIN_MENU. |  
| Procedure | <ol><li>Initialize new game data.</li> <li>Load new game data.</li> <li>Game state is switched to INSTRUCTIONS(*g*) where *g* is the game name.</li></ol> |  
| Outcome | Player is in a mini-game instance of game *g*. |  

| [PUC No.2](#puc-2) | Event: Exit Game |  
| ---- | ------- | 
| Trigger | Player selects "Exit" option. |  
| Preconditions | Game state is IN_MAIN_MENU, FINISHED(*g*), or PAUSE(*g*) for some game name *g*. |
| Procedure | <ol><li>Display message "Do you really want to exit?".</li> <li>Get input from (1).</li> <li>Close the tab of the web application depending on the input.</li></ol>|  
| Outcome | Player exits the web application. |  

| [PUC No.3](#puc-3) | Event: Pause Game | 
| ---- | ------- | 
| Trigger | Player selects "Pause" option. |  
| Preconditions | Player is in a mini-game *g*, and the game state is ACTIVE(*g*). |
| Procedure | <ol><li>Game is switched into PAUSE(*g*) state.</li> <li>A pause menu is displayed.</li></ol> |  
| Outcome | Game is paused and a pause menu is on the screen. |  

| [PUC No.4](#puc-4) | Event: Resume Game | 
| ---- | ------- | 
| Trigger | Player selects "Resume" option from pause menu. |  
| Preconditions | Player is in mini-game *g*, and the game state is PAUSE(*g*). |  
| Procedure | <ol><li>Game is switched into ACTIVE(*g*) state.</li> <li>Pause menu is not displayed.</li></ol> |    
| Outcome | Game is resumed and players can play the game. |  

| [PUC No.5](#puc-5) | Event: End Game |  
| ---- | ------- | 
| Trigger | Player selects "End" option from pause menu. |  
| Preconditions | Player is in mini-game *g*, and the game state is PAUSE(*g*). |
| Procedure | <ol><li>Game is switched into FINISHED(*g*) state.</li> <li>End screen is displayed.</li></ol> |   
| Outcome | Game is terminated and the end screen is on the screen. |  

| [PUC No.6](#puc-6) | Event: Check Final Score |  
| ---- | ------- | 
| Trigger | The end screen is displayed. |  
| Preconditions | Game state is in FINISHED(*g*) for some game *g*, and the end menu is displayed.|
| Procedure | <ol><li>The final score will be displayed.</li></ol> |    
| Outcome | The final score of the player is displayed.|  

| [PUC No.7](#puc-7) | Event: Check Ability Report |  
| ---- | ------- | 
| Trigger | The end menu is displayed. |  
| Preconditions | Game state is FINISHED(*g*) or IN_MAIN_MENU. |
| Procedure | <ol><li>The level of competency will be generated for each ability. </li></ol> |    
| Outcome | The ability report of the player is displayed. |

| [PUC No.8](#puc-8) | Event: Move Character | 
| ---- | ------- | 
| Trigger | Player presses a key *k* to move the character.  |  
| Preconditions |The game state is ACTIVE(SINKHOLE).|
| Procedure | <ol><li>Character moves according to the pressed key.</li></ol> |    
| Outcome | Character's direction in 2-dimensional space is changed depending on the key *k*  until the key is released. The specific key *k* determines whether the character moves left or right. | 

| [PUC No.9](#puc-9) | Event: Get Hit | 
| ---- | ------- | 
| Trigger | Character is hit by an asteroid or falls through a sinkhole. | 
| Preconditions | The game state is ACTIVE(SINKHOLE).  |
| Procedure | <ol><li>Game is switched into the FINISHED(SINKHOLE) state.</li> <li>End screen is displayed.</li></ol> |    
| Outcome | Game is terminated and the end screen is on the screen. | 

| [PUC No.10](#puc-10) | Event: Play the Piano | 
| ---- | ------- | 
| Trigger | Player presses a key *k* for a certain musical note.  |  
| Preconditions | Current game is ACTIVE(PIANIST). |
| Procedure | <ol><li>The sound of the pressed musical note corresponding to the key *k* is played.</li> <li>Determine if the musical note is correct.</li></ol> |    
| Outcome | <ol><li>Player gets scored depending on the result</li><li>Are there more notes to be played?<ul><li>YES: VOID</li><li>NO: Are there more rounds to be played?<ul><li>YES: Go to the next round</li><li>NO: End game, change state to FINISHED(PIANIST), show the end screen.</ul></li></ul></ol> | 

| [PUC No.11](#puc-11) | Event: Select the Cup | 
| ---- | ------- | 
| Trigger | Player clicks on the cup that they think has the object underneath it |  
| Preconditions | The player has been shown the object, the object was placed underneath one of the three cups, and the cup positions on the screen are done shuffling, and current state is ACTIVE(CUP_SWAP). |
| Procedure | <ol><li>Game is switched into FINISHED(CUP_SWAP).</li></ol>  |    
| Outcome | <ol><li>Player gets scored depending on if they chose correctly</li><li>Are there more rounds to be played?<ul><li>YES: Go to the next round</li><li>NO: End game, change state to FINISHED(CUP_SWAP), show the end screen.</ul></li></ol> | 

| [PUC No.12](#puc-12) | Event: Select the Green Light | 
| ---- | ------- | 
| Trigger | Player presses a key *k* on the keyboard. |  
| Preconditions | Current game is ACTIVE(GREEN_LIGHT_GO). |
| Procedure | <ol><li>Game is switched into FINISHED(GREEN_LIGHT_GO) state.</li></ol>  |    
| Outcome | <ol><li>Player gets scored depending on the lit bulb's distance to the target bulb.</li><li>Are there more rounds to be played?<ul><li>YES: Go to the next round.</li><li>NO: End game, change state to FINISHED(GREEN_LIGHT_GO),and show the end screen.</ul></li></ol> | 

| [PUC No.13](#puc-13) | Event: Show Game Instructions |  
| ---- | ------- | 
| Trigger | Player has selected game *g* |  
| Preconditions | Game state is IN_MAIN_MENU. |
| Procedure | <ol><li>Game state is switched to INSTRUCTIONS(*g*).</li></ol> |    
| Outcome | Instructions are shown for the game *g*. | 


| [PUC No.14](#puc-14) | Event: Dismiss Game Instructions | 
| ---- | ------- | 
| Trigger | Player presses the "OK" button on the instruction panel. |  
| Preconditions | Game state is INSTRUCTIONS(*g*) for some game *g*. |
| Procedure | <ol><li>Game state is switched to ACITVE(*g*).</li></ol> |    
| Outcome | Player can start playing the game *g*. |


| [PUC No. 15](#puc-15) | Event: Flip First Card | 
| ---- | ------- | 
| Trigger | Players select a card *c1* on the screen.  |  
| Preconditions | Current game state is ACTIVE(FLIP_FLOP_CLEAR), and there is no card flipped. |
| Procedure | <ol><li>Display the face of card *c1*.</li></ol>|
| Outcome |The card *c1* remains face up.|


| [PUC No.16](#puc-16) | Event: Flip Second Card | 
| ---- | ------- | 
| Trigger | Players select a card *c2* on the screen.  |  
| Preconditions |Current game state is ACTIVE(FLIP_FLOP_CLEAR), and the first card *c1* is flipped. |
| Procedure | <ol><li>Flip and display the face of card *c2*.</li> <li>Check the type of the first card *c1*.</li></ol>  |    
| Outcome | The cards *c1* and *c2* remain face up if the face of *c1* is the same type as that of *c2*; otherwise the card *c1* and *c2* go face down.|


| [PUC No.17](#puc-17) | Event: Flip Third Card | 
| ---- | ------- | 
| Trigger | Players select a card *c3* on the screen.  |  
| Preconditions | Current game is ACTIVE(FLIP_FLOP_CLEAR), and the second card *c2* is flipped |
| Procedure | <ol><li>Display the face of flipped card *c3*.</li> <li> Check the type of the second card *c2*.</li></ol>  |    
| Outcome |The card *c3* remains face up if its face is of the same type of the second one *c2*; otherwise the card *c3* together with the first card *c1* and the second card *c2* go face down. |

| [PUC No.18](#puc-18) | Event: A Type Is Cleared | 
| ---- | ------- | 
| Trigger | Flip Third Card event is occurring.  |  
| Preconditions | Current game is ACTIVE(FLIP_FLOP_CLEAR), and the third card *c3* is face up. |
| Procedure | <ol><li>The card type that corresponds to the type of card *c3* is cleared.</li><li>The cards *c1*, *c2*, and *c3* disappear from the game board. |    
| Outcome | The player's score is increased. The cleared cards disappear.|


| [PUC No.19](#puc-19) | Event: Check Auto End | 
| ---- | ------- | 
| Trigger | A Type Is Cleared event is occurring.  |  
| Preconditions | Current game is ACTIVE(FLIP_FLOP_CLEAR), and there does not exist any card *c* on the game board. |
| Procedure | <ol><li> Game state changes to END(*g*).|    
| Outcome | End of the Flip-Flop Clear Game. |

| [PUC No.20](#puc-20) | Event: Open Change Settings  | 
| ---- | ------- | 
| Trigger | Player clicks on the settings icon.|  
| Preconditions | Game state *s* is PAUSE(*g*) for some game *g* or IN_MAIN_MENU. |
| Procedure | <ol><li>Game state is changed to SETTINGS(*s*).</li><li>The settings menu appears.</li></ol> |
| Outcome | Player can alter game settings, such as volume and brightness. | 

| [PUC No.21](#puc-21) | Event: Alter Settings  | 
| ---- | ------- | 
| Trigger | The user changes the value of a setting *t* such as volume or brightness. |  
| Preconditions | Game state is SETTINGS(*s*) for some state *s*. |
| Procedure | <ol><li>Game setting *t* is changed.</li></ol> |
| Outcome | Game settings are changed. | 

| [PUC No.22](#puc-22) | Event: Close Settings | 
| ---- | ------- | 
| Trigger | Player presses the key to close the settings menu. |  
| Preconditions | Game state is SETTINGS(*s*) for some state *s*. |
| Procedure | <ol><li>Game state is changed to state *s*.</li></ol> |
| Outcome | Player returns to screen that they were on before they entered the settings menu. |

| [PUC No.23](#puc-23) | Event: Exit End Screen | 
| ---- | ------- | 
| Trigger | Player presses exit button on the end screen. |  
| Preconditions | Game state is END(*g*). |
| Procedure | <ol><li>Game state is changed to IN_MAIN_MENU.</li><li>The screen changes to the main menu.</li></ol> |
| Outcome | The player is returned to the main menu. |


### 1.6 Definitions, Acronyms and Abbreviations
#### Domain-specific terminology
* Audial perception: The ability to receive and interpret information that reaches ears through audible frequency waves transmitted through the air or other means.
* Cognitive abilities: Brain-based skills which are needed in acquisition of knowledge, manipulation of information, and reasoning.
* Competency profile: The set of cognitive and motor abilities that characterize a task.
* Gameplay challenges: Any in-game activity with a success condition which engages the player in a way that requires some level of proficiency in at least one dimension (physical or cognitive).
* Gameplay mechanics: The game's rules, objectives, challenges and methods that the player is meant to use or follow to interact with the game.
* Mechanical achievability: A notion whether a player can complete a game's challenges given their cognitive and motor abilities.
* Mechanical difficulty: A notion how the design of the game itself affects this mechanical achievability.
* Mechanical experience: The underlying interaction between the user and game.
* Motor abilities: Learned abilities to cause a predetermined movement outcome with maximum certainty.
* Object Recognition: The ability to perceive an object's physical properties and apply semantic attributes to the object, which includes the understanding of its use.
* OS: Operating System. The system software that manages computer hardware, software resources, and provides common services for computer programs.
* Parameters: A numerical value or property of the software that helps determine the environment or state of the software.  
* PC: Personal Computer. A multi-purpose machine whose size, capabilities, and price make it feasible for individual use.
* Selective visual: The ability to focus attention intentionally on a visual stimuli.
* Time to contact: The ability to press something when you are supposed to.
* Token change direction: The ability to determine when to change direction.
* Unity: A cross-platform game engine developed by Unity Technologies.
* UI: User Interface. The method through which users interact with the hardware and software of computers and other electronic devices.

#### Document-specific terminology
* Mid-range modern computer: We consider a mid-range modern computer to be a laptop or desktop from 2015 or newer with a 2.9 GHz Intel Core i5 or better processor, 8GB or more of 1867 MHz DDR3 memory, and a integrated GPU that performs at the level of or better than an Intel Iris Pro 5200.
* N/A: Indicates that a section is not applicable.
* SRS: Software Requirements Specification. This requirements document.


### 1.7 References
“Unity (Game Engine).” Wikipedia. Wikimedia Foundation, October 2, 2020.    https://en.wikipedia.org/wiki/Unity_(game_engine).  
“User Interface.” Dictionary.com. Dictionary.com. Accessed October 12, 2020, https://www.dictionary.com/browse/user-interface.  
“Operating System.” Wikipedia. Wikimedia Foundation, October 7, 2020.   https://en.wikipedia.org/wiki/Operating_system.   
“Personal Computer.” Wikipedia. Wikimedia Foundation, October 7, 2020. https://en.wikipedia.org/wiki/Personal_computer.   
“Cognitive Skill.” Wikipedia. Wikimedia Foundation, September 13, 2020.      https://en.wikipedia.org/wiki/Cognitive_skill.  
“Motor Skill.” Wikipedia. Wikimedia Foundation, September 22, 2020. https://en.wikipedia.org/wiki/Motor_skill.   
Soraine, Sasha. “CS 4ZP6: Mini-Game Battery Project.” 2020. PDF file.  
Robertson, James P. and S. Robertson. “Volere: Requirements specification template.” 2000. PDF file.  

### 1.8 Document Overview
The rest of this document will be divided into 4 parts. The first part is the product overview which consists of topics such as the product perspective, functions, and constraints. This part describes information about the general factors of the product and requirements. The second part describes the requirements related to the external interface, functional requirements, and quality of service. This part illustrates the detailed and specific requirements of the product. The third part relates to verification which provides the verification approaches and methods that are going to determine whether the software is successful at solving the problems posed by the stakeholders. The last part describes the appendix.

## 2. Product Overview
Our product is composed of a series of small video games that focuses on five select cognitive abilities out of [46 identified human cognitive and psychomotor requirements](#111-identified-cognitive-and-motor-abilities) associated with video game challenges: object recognition, token change direction, time to contact, audial perception, and selective visual. Each individual game will focus primarily on one of the five selected abilities. The games will be accessible by the players through a web application that runs on a web server, and collects input through the keyboard and mouse. The input collected by the web application will be used to establish an ability baseline for the player, and to determine how parameter changes in the atomic challenges affect mechanical achievability and mechanical difficulty. The research results from the product will be used to contribute to the creation of innovative approaches in future game development and will bring about a greater understanding of the underlying science behind video games.

### 2.1 Product Perspective
The G-ScalE project has existing research by [Sasha Soraine](@sorainsm), who has created a framework for identifying atomic challenges in video games and hypotheses about the mechanical experiences of video games.  Our capstone group is one of two groups that will be working on products to test and further the hypotheses and research already done. The two groups will be sharing the backend framework and will design and implement it such that the video game designs from both groups adhere to it.  The video game designs will reference existing test methods and research of the selected cognitive abilities to design video games that can be used to test and capture data related to the cognitive abilities.

### 2.2 Product Functions
* User must understand how to navigate through the application using the user interface
* User must understand how to play the game
* The user should be able to complete each mini game without major software errors that prevent the user from completing the game
* The mini game must be completed in under a minute
* Sufficient data should be able to be collected from each mini game

### 2.3 Product Constraints

The only external devices that can be used to collect user data at this point in time is the computer keyboard and computer mouse, due to the ongoing coronavirus pandemic. As such, our product's software must be accessed as a web application that can be reached using a standard internet browser on a laptop or desktop.

### 2.4 User Characteristics
* Proficient with use of a computer mouse and computer keyboard
* Able to read and understand the instructions of the game
* Has access to the internet and has a browser that can run the web application
* Has a computer mouse and computer keyboard
* Player is a generic able-bodied neurotypical person, a player with
normative motor and cognitive abilities

### 2.5 Assumptions and Dependencies

* Free-to-use assets from the Unity asset store will remain free to use during project implementation and after project completion
* The license requirements for the software being developed and Unity do not change
* The expectations required by the project lead do not change
* The product will not be commercially released
* Game assets may come from third party resources
* There exist effective methods to measure a subset of the cognitive and motor abilities using the keyboard only as the main input device

## 3. Requirements

### 3.1 External Interfaces

#### 3.1.1 User interfaces
        
##### Ease of Use Requirements   
| **ID**:  EU-1        | **Type**: Non-Functional (Ease of Use)  |      
| -------------------- | --------------------------------------- |      
| **Related PUC**: N/A | **Originator**: [Yunfei](@yangy113)                  |      
| **Description**      | The player should know how to control all the mini games after a relatively short game time.                      |    
| **Rationale**        | The control of mini games should not make players confused or frustrated.                                  |    
| **Fit Criterion**    | More than 85% of players can master these games after reading the provided game instructions as measured by a user survey.     |   
| **Priority**         | Medium                              |    

| **ID**:  EU-2        | **Type**: Non-Functional (Ease of Use)  |      
| -------------------- | --------------------------------------- |      
| **Related PUC**: N/A | **Originator**: [Yunfei](@yangy113)                  |      
| **Description**      | Players from all ages can enjoy playing these games. |  
| **Rationale**        | Collecting game data from different age groups is very important for measuring motor and cognitive abilities of different individuals.                                             |    
| **Fit Criterion**    | General population of players aged about 10 to 70 can all play these games because game difficulty is designed to match this demographic. |   
| **Priority**         | High                                |   
      
##### Personalization Requirements     
| **ID**:  PR-1        | **Type**: Non-Functional (Personalization)  |      
| -------------------- | ------------------------------------------- |      
| **Related PUC**: N/A | **Originator**: [Yunfei](@yangy113)                      |      
| **Description**      | In each mini game, players can modify the game settings to adapt to their needs for volume and window size.      |    
| **Rationale**        | Each player has their preference on setting options. |     
| **Fit Criterion**    | Setting options can be adjusted.        |   
| **Priority**         | Low                                     |    
       
##### Learning Requirements  
| **ID**:  LR-1        | **Type**: Non-Functional (Learning)      |      
| -------------------- | ---------------------------------------- |      
| **Related PUC**: N/A | **Originator**: [Yunfei](@yangy113)                    |      
| **Description**      | Players can learn these games quickly, making the motor and cognitive ability aptitudes reflected by the data obtained accurate.   |    
| **Rationale**        | The motor and cognitive abilities reflected by the data should not be made inaccurate because the player does not understand the game. |    
| **Fit Criterion**    | More than 85% of players can master these games after reading the provided game instructions as measured by a user survey.        |   
| **Priority**         | Medium           |    

| **ID**:  LR-2        | **Type**: Non-Functional (Learning)      |      
| -------------------- | ---------------------------------------- |      
| **Related PUC**: N/A | **Originator**: [Alice](@ipa1)                    |      
| **Description**      | Players can quickly understand how to navigate through the menus of our web application.   |    
| **Rationale**        | We need to consider the web application as a whole, rather than just the mini-games, when it comes to learnability or otherwise the player will not play our games if they get lost in menus. |
| **Fit Criterion**    | Design an menu that 85% of players find easy to learn how to navigate through the web application as indicated in a post-game user survey.     |   
| **Priority**         | Medium           |    
       
##### Understandability and Politeness Requirements  
| **ID**:  UP-1        | **Type**: Non-Functional (Understandability and Politeness)      |      
| -------------------- | ---------------------------------------- |      
| **Related PUC**: N/A | **Originator**: [Yunfei](@yangy113)                    |      
| **Description**      | All text related to settings and rules that appear in mini games must be in English.   |    
| **Rationale**        | The target players of these mini games are in English-speaking countries, so the language of the games should be in English. |    
| **Fit Criterion**    | All text related to settings and rules that appear in mini games are written in English. |   
| **Priority**         | High                               |   

| **ID**:  UP-2        | **Type**: Non-Functional (Understandability and Politeness)      |      
| -------------------- | ---------------------------------------- |      
| **Related PUC**: N/A | **Originator**: [Yunfei](@yangy113)                    |      
| **Description**      | Symbols and icons that appear in mini games should be easy to understand.                                    |    
| **Rationale**        | The gameplay should not be influenced by complicated and obscure symbols.                                  |    
| **Fit Criterion**    | More than 85% symbols and icons that appear in the mini games convey intuitive information, as indicated in a post-game user survey.                               |   
| **Priority**         | Medium                                   |  

##### Accessibility Requirements
| **ID**:  AR-1        | **Type**: Non-Functional (Accessibility) |    
| -------------------- | ---------------------------------------- |      
| **Related PUC**: N/A | **Originator**: [Yunfei](@yangy113)                    |      
| **Description**      | Notify the player with the ability required to play a game before the game starts. |    
| **Rationale**        | People with disabilities may not be able to complete certain games, but there are still some games that they can play.  |     
| **Fit Criterion** | Some games that have special requirements for players need to be noted. For example, games about audial perception need to explain that hearing impaired players cannot participate, and certain games about motor abilities need to explain that different kinds of disabled people cannot participate. In other words, except for those marked contents, disabled people can play other games. |   
| **Priority**         | High                               |    


#### 3.1.2 Hardware interfaces

| **ID**:  HI-1        | **Type**: Non-Functional                 |      
| -------------------- | ---------------------------------------- |      
| **Related PUC**: N/A | **Originator**: [Yunfei](@yangy113)                    |      
| **Description**      | All games should be played on mid-range modern computers.   |    
| **Rationale**        | In order to make the games run better, the computer must not be too old.   |    
| **Fit Criterion**    | The games run on mid-range modern computers. On computers with lower specifications than that of a mid-range modern computer, we make no guarantee that the games will run.     |   
| **Priority**         | High                               |   

| **ID**:  HI-2        | **Type**: Non-Functional                 |      
| -------------------- | ---------------------------------------- |      
| **Related PUC**: N/A | **Originator**: [Yunfei](@yangy113)                    |      
| **Description**      | The Internet speed of the player's computer must be at least 10 Mbps.  |    
| **Rationale**        | In order to ensure the player receives optimal and smooth gameplay performance, they need to maintain a good Internet connection.   |    
| **Fit Criterion**    | Test the Internet speed of our players and give them a warning about potential gameplay issues if their Internet speed is lower than 10 Mbps.    |   
| **Priority**         | High                               |   

| **ID**:  HI-3        | **Type**: Non-Functional                 |      
| -------------------- | ---------------------------------------- |      
| **Related PUC**: N/A | **Originator**: [Yunfei](@yangy113)                    |      
| **Description**      | The resolution of computers should be proper to run these games.   |    
| **Rationale**        | In order to make the games run better, the resolution of computers should not be too bad.   |    
| **Fit Criterion**    | The resolution of computers running these games should be at least 1024×768.     |   
| **Priority**         | High                               |   

| **ID**:  HI-4        | **Type**: Non-Functional                 |      
| -------------------- | ---------------------------------------- |      
| **Related PUC**: N/A | **Originator**: [Yunfei](@yangy113)                    |      
| **Description**      | The keyboard of computers running these games should be in good condition.   |    
| **Rationale**        | The problem of the keyboard itself can interfere with the results of measuring motor and cognitive abilities.   |    
| **Fit Criterion**    | The keyboards of computers running these games should have no missing keys and no sticky keys.   |   
| **Priority**         | High                               |   

| **ID**:  HI-5        | **Type**: Non-Functional                 |      
| -------------------- | ---------------------------------------- |      
| **Related PUC**: N/A | **Originator**: [Yunfei](@yangy113)                    |      
| **Description**      | The mouse of computers running these games should be in good condition.   |    
| **Rationale**        | The mouse, although not the primary input source for the mini-games, is used to navigate the menus of our web application.  |    
| **Fit Criterion**    | The mouse of computers running these games should be responsive and its latency should be at most 4 milliseconds.     |   
| **Priority**         | Medium                               |   

| **ID**:  HI-6        | **Type**: Non-Functional                 |      
| -------------------- | ---------------------------------------- |      
| **Related PUC**: N/A | **Originator**: [Yunfei](@yangy113)                    |      
| **Description**      | The RAM of computers should be proper to run these games.   |    
| **Rationale**        | In order to make the games run better, the RAM of computers should not be too bad.   |    
| **Fit Criterion**    | The RAM of computers running these games should be at least 8GB.     |   
| **Priority**         | High                               |   

| **ID**:  HI-7        | **Type**: Non-Functional                 |      
| -------------------- | ---------------------------------------- |      
| **Related PUC**: N/A | **Originator**: [Yunfei](@yangy113)                    |      
| **Description**      | Computers running these games should have enough storage. |    
| **Rationale**        | In order to make the games run better, the storage of computers should not be too bad.   |    
| **Fit Criterion**    | The storage of computers running these games should have at least 50 GB of free disk space.     |   
| **Priority**         | High                               |   

#### 3.1.3 Software interfaces


| **ID**:  SI-1        | **Type**: Non-Functional                 |      
| -------------------- | ---------------------------------------- |      
| **Related PUC**: N/A | **Originator**: [Yunfei](@yangy113)                    |      
| **Description**      | Game functionality will be limited to what is supported by the Unity platform. |    
| **Rationale**        | According to the needs of this project, all games should be run on the Unity platform.   |    
| **Fit Criterion**    | Unity must be used to create mini games. No other game engine or library will be used.    |   
| **Priority**         | Very High                               |   


### 3.2 Functional
#### General functional requirements

| **ID**: FG-1         | **Type**: Functional        |
|-----------------------|--------------------|
| **Related PUC**: [1](#puc-1)         | **Originator**: [Alice](@ipa1)   |
| **Description**          | The player must have the ability to choose the game they play.|
| **Rationale**            | The player may have some preference on which game they want to play .  |
| **Fit Criterion**         | The player can scroll through the games in the collection, and can press a button when the game they want to play is selected.  |
| **Priority**              | High     |

| **ID**: FG-2         | **Type**: Functional        |
|-----------------------|--------------------|
| **Related PUC**: [3](#puc-3)        | **Originator**: [Alice](@ipa1)    |
| **Description**          | The player must be able to pause the game.|
| **Rationale**            | The player may want to take a break, or change game settings, or exit the current mini game. |
| **Fit Criterion**         | The player can press the pause button to pause the game.  |
| **Priority**              | High     |

| **ID**: FG-3         | **Type**: Functional        |
|-----------------------|--------------------|
| **Related PUC**: [5](#puc-5)        | **Originator**: [Alice](@ipa1)    |
| **Description**          | The player must have the ability to exit the current mini game.|
| **Rationale**            | The player may wish to stop playing the mini game.|
| **Fit Criterion**         | The player can choose to exit the mini game from the pause menu. |
| **Priority**              | High     |

| **ID**: FG-4          | **Type**: Functional        |
|-----------------------|--------------------|
| **Related PUC**: [14](#puc-14)       | **Originator**: [Alice](@ipa1)    |
| **Description**          | After the instructions for the game are given, the game must start when the player is ready to play the game.|
| **Rationale**            | The player may need some time to read and understand the instructions   |
| **Fit Criterion**         | There is a button on the instructions screen that the player can press when they are ready to play the game  |
| **Priority**              | High     |

| **ID:** FG-5 | Type: Functional |
|-----------------------|--------------------|
| **PUC**: [7](#puc-7) | Originator: [Meijing](@lim147) |
| **Description** | At the end of games, the player is able to get a testing report of all abilities.|
| **Rationale** | The player may be curious about the testing result of testing results of motor and cognitive abilities. |
| **Fit Criterion** | A participant ability profile will be generated to show the level of competency the player has in the tested abilities. |
| **Priority** | High |

| **ID**: FG-6         | **Type**: Functional        |
|-----------------------|--------------------|
| **Related PUC**: [6](#puc-6)        | **Originator**: [Alice](@ipa1)    |
| **Description**          | The player must be able to see their score for the current game|
| **Rationale**            | The player may wish to see how well they are doing in the games  |
| **Fit Criterion**         | The score of the player is stored after each round of a game instance, and is displayed at the end of the round.  |
| **Priority**              | High     |


| **ID**: FG-7         | **Type**: Functional        |
|-----------------------|--------------------|
| **Related PUC**: [13](#puc-13)        | **Originator**: [Alice](@ipa1)    |
| **Description**          | The player must know how to play the game.|
| **Rationale**            | The player needs to know how to play the game so that data can be collected.   |
| **Fit Criterion**         | The instructions for the game will appear after selecting the game from the game selection menu.  |
| **Priority**              | High     |




#### Functional requirements for specific games



##### Game: Cup Swap

| **ID**: FCS-1         | **Type**: Functional        |
|-----------------------|--------------------|
| **Related PUC**: N/A       | **Originator**: [Alice](@ipa1)    |
| **Description**          | The Cup Swap game must show the object, and cups and the swapping of the cups.|
| **Rationale**            | The player needs to see the objects and cups to decide which cup has the object. |
| **Fit Criterion**         | The object and cups will be displayed and animated on the screen using graphical assets.  |
| **Priority**              | High     |

| **ID**: FCS-2         | **Type**: Functional        |
|-----------------------|--------------------|
| **Related PUC**: N/A        | **Originator**: [Alice](@ipa1)    |
| **Description**          | The Cup Swap game must randomize the swapping of the cups.|
| **Rationale**            | The cognitive ability being tested requires tracking the object, and the correct answer should not be memorizable. |
| **Fit Criterion**         | A random number generator will be used to randomize the swapping.  |
| **Priority**              | High     |

| **ID**: FCS-3        | **Type**: Functional        |
|-----------------------|--------------------|
| **Related PUC**: [11](#puc-11)        | **Originator**: [Alice](@ipa1)    |
| **Description**          | The Cup Swap game must enter the next round if the player is not in the last round.|
| **Rationale**            | The game should start off easy and increase in difficulty to measure the competency of the player in the selective visual cognitive ability. |
| **Fit Criterion**         | The player has the option to press a button to continue to the next round.  |
| **Priority**              | High     |

| **ID**: FCS-4        | **Type**: Functional        |
|-----------------------|--------------------|
| **Related PUC**: [11](#puc-11)        | **Originator**: [Alice](@ipa1)    |
| **Description**          | The Player should be able to choose the cup that has the object.|
| **Rationale**            | The player must provide input for the game to progress and to get a player score. |
| **Fit Criterion**         | The player has the option to press a button to indicate their guess |
| **Priority**              | High     |


##### Game: Pianist

| **ID**: FP-1         | **Type**: Functional        |
|-----------------------|--------------------|
| **Related PUC**: N/A        | **Originator**: [Shuo](@hes11)    |
| **Description**          | The Pianist game must play a tune clearly, and let the player know when to respond. |
| **Rationale**            | The player needs to listen to a musical tune first before duplicating it. |
| **Fit Criterion**         | The sound of the musical will be played from a speaker using auditory assets.  |
| **Priority**              | High     |

| **ID**: FP-2         | **Type**: Functional        |
|-----------------------|--------------------|
| **Related PUC**: N/A        | **Originator**: [Shuo](@hes11)    |
| **Description**          | The Pianist game must randomize the order of the musical notes in the tune.|
| **Rationale**            | The order of the tune should not be memorable.|
| **Fit Criterion**         | A random musical notes generator will be used to randomize the musical notes.  |
| **Priority**              | High     |

| **ID**: FP-3         | **Type**: Functional        |
|-----------------------|--------------------|
| **Related PUC**: [10](#puc-10)        | **Originator**: [Shuo](@hes11)    |
| **Description**          | The player must be able to enter different musical notes from the keyboard using different keys. |
| **Rationale**            | Player needs to recognize different keys corresponding to different musical notes to play back the the music. |
| **Fit Criterion**         | Each musical note that will be used in the music section will be linked to a certain key on the keyboard.  |
| **Priority**              | High     |



##### Game: Green Light Go

| **ID**: FGLG-1         | **Type**: Functional        |
|-----------------------|--------------------|
| **Related PUC**: [12](#puc-12) | **Originator**: [Yunfei](@yangy113)    |
| **Description**          | The Green Light Go game must show the light bulbs, and how they light up. |
| **Rationale**            | The player needs to see the lights to play the game. |
| **Fit Criterion**         | The lights will be displayed and animated on the screen using graphical assets.  |
| **Priority**              | High     |

| **ID**: FGLG-2         | **Type**: Functional        |
|-----------------------|--------------------|
| **Related PUC**: N/A        | **Originator**: [Yunfei](@yangy113)    |
| **Description**          | The Green Light Go game must light up sequentially. |
| **Rationale**            | The cognitive ability being tested is time to contact, so the lights need to move sequentially so that the player can track the movement of the lights and press the key at the appropriate time.|
| **Fit Criterion**         | The lights will be lit up sequentially.  |
| **Priority**              | High     |

| **ID**: FGLG-3         | **Type**: Functional        |
|-----------------------|--------------------|
| **Related PUC**: N/A        | **Originator**: [Yunfei](@yangy113)    |
| **Description**          | The speed of the lights should increase when the Green Light Go game gets into the next round. |
| **Rationale**            | Faster lights can make the game more difficult. |
| **Fit Criterion**         | The speed of the lights should increase.  |
| **Priority**              | High     |

##### Game: Holy Sinkholes!

| **ID**: FHS-1         | **Type**: Functional        |
|-----------------------|--------------------|
| **Related PUC**: [8](#puc-8)        | **Originator**: [Jack](@bucklj4)    |
| **Description**          | Players must be able to move only in the left or right direction using two distinct keys on the keyboard for the game Holy Sinkholes!  |
| **Rationale**            | We are testing the token change direction ability, so we want to reduce the possible directions the user can move in as much as possible. The simplest approach is to only allow the user to change directions in a binary fashion: from left to right or right to left. 
| **Fit Criterion**         | The player can only move their character left or right through the use of two unique keys on the keyboard. |
| **Priority**                | High     |

| **ID**: FHS-2         | **Type**: Functional        |
|-----------------------|--------------------|
| **Related PUC**: [8](#puc-8)        | **Originator**: [Jack](@bucklj4)    |
| **Description**          | When players move their character all the way to the left hand side of the screen, their character must be warped to the right hand side of the screen and vice-versa.  |
| **Rationale**            | We are testing the token change direction ability, so we want the player to continuously make decisions about direction. By warping the player's character, we force players to be continuously on the move. | 
| **Fit Criterion**         | The player's character gets warped to the opposite side of the screen when the character is moved to one of the screen's edges. |
| **Priority**                | High     |

| **ID**: FHS-3         | **Type**: Functional        |
|-----------------------|--------------------|
| **Related PUC**: N/A        | **Originator**: [Jack](@bucklj4)    |
| **Description**          | The sink holes and garbage asteroids must appear in random locations of the screen during gameplay in the game Holy Sinkholes! |
| **Rationale**            | We want to record how stimuli in the game (the sinkholes and asteroids) affect the player's decision to move left and right and how quickly they can perform this action. If we do not randomize the location of sinkholes and asteroids, then the player can simply memorize which direction they should move towards during gameplay, defeating the purpose of our measurements.   |
| **Fit Criterion**         | The sink holes and garbage asteroids appear in random locations of the screen during gameplay in the game Holy Sinkholes!  |
| **Priority**              | High     |

| **ID**: FHS-4         | **Type**: Functional        |
|-----------------------|--------------------|
| **Related PUC**: [8](#puc-8)        | **Originator**: [Jack](@bucklj4)    |
| **Description**          | There must always exist a safe spot for the player to move to such that they will avoid the sinkholes and asteroids in the game Holey Sinkholes! |
| **Rationale**            | We want to determine how well the user can change the direction of the character to avoid obstacles. If there comes a point where there is no such possible way for the user to avoid the obstacles, then we cannot objectively determine how well the user performed since any move they would have made would have resulted in the game ending.  |
| **Fit Criterion**         | There must always be a way for the user to move from their current, by moving left and right, to avoid the sinkholes and asteroids in the game Holey Sinkholes.   |
| **Priority**              | High     |


##### Game: Flip-Flop Clear

| ID: FFC-1 | Type: Functional |
|-----------|--------------------|
| PUC: N/A | Originator: [Meijing](@lim147) |
| Description |The Flip-Flop Clear Game must show cards on the screen and be able to display both sides of the card during the game.|
| Rationale | The player must see the cards to interact with the game.|
| Fit Criterion |  At the beginning of the game, all cards remain face down. In the gameplay, some cards can be flipped and show the faces on the screen. The cards will be displayed on the screen using the graphical assets.|
| Priority | High |

| ID: FFC-2 | Type: Functional |
|-----------|--------------------|
| PUC: [15](#puc-15), [16](#puc-16), [17](#puc-17) | Originator: [Meijing](@lim147) |
| Description | In Flip-Flop Clear gameplay, the player must be able to flip the card when selecting the card.|
| Rationale | Each card has a different image on it.  |
| Fit Criterion | The player will be able to see the image when the card is selected. The Flip-Flop Clear Game must be able to identify which key is pressed and trigger the state change of the card associated to the key. |
| Priority | High |

| ID: FFC-3 | Type: Functional |
|-----------|--------------------|
| PUC: N/A | Originator: [Meijing](@lim147) |
| Description | Cards should flip or remain unflipped when they are supposed to. |
| Rationale | Cards in card games only have two sides, and the orientation of the card is a key element in the Flip-Flop Clear game. |
| Fit Criterion | The system will only react if a player presses the key that associates to the card. Any other key pressing will not change the state of the game.|
| Priority | High |

|ID: FFC-4 | Type: Functional |
|-----------|--------------------|
| PUC: N/A | Originator: [Meijing](@lim147) |
| Description | The Flip-Flop Clear game must randomly choose three types of images, randomly choose three images from that type, and randomly place the cards on the screen in a 3 x 3 array. |
| Rationale | The Object Recognition ability being tested requires perceiving an object’s physical properties and applying semantic attributes to the object, and the placement of the cards should not be memorizable. |
| Fit Criterion | The system will randomly pick object images from an image base which is big enough to guarantee that different images are picked for different rounds of the game. The chance to get the same images in same order should be lower than 0.001%. |
| Priority | High |

| ID: FFC-5 | Type: Functional |
|-----------|--------------------|
| PUC: [19](#puc-19) | Originator: [Meijing](@lim147) |
| Description | The Flip-Flop Clear game automatically finishes when all cards remain disappear. |
| Rationale | In this situation, all images of different kinds are correctly identified and the game should be terminated. |
| Fit Criterion | All cards will be gone when the game finishes. |
| Priority | High |



### 3.3 Quality of Service
#### 3.3.1 Performance
| **ID**: PER-1          | **Type**: Non-functional         |
|-----------------------|--------------------|
| **Related PUC**: N/A        | **Originator**: [Jack](@bucklj4)   |
| **Description**          | Each mini-game must maintain a frame rate of at least 30 frames per second on a mid-range modern computer.|
| **Rationale**            | In order for games, especially mini-games, to feel interactive they have to maintain a fluid frame rate. In commercial games, 30 frames per second is [considered the minimum acceptable frame rate for playability](https://www.logicalincrements.com/articles/framerate ).  |
| **Fit Criterion**         | Each mini-game will be benchmarked on a mid-range modern computer by having the mini-game played five times. For each mini-game, for each of those five times, the monitored frame rate must not drop below 30 frames per second.  |
| **Priority**              | Very High      |

| **ID**: PER-2          | **Type**: Non-functional         |
|-----------------------|--------------------|
| **Related PUC**: N/A        | **Originator**: [Jack](@bucklj4)   |
| **Description**          | Each mini-game must load to a playable state within 10 seconds from the time the user indicates which game they wish to play on the main menu.|
| **Rationale**            | Mini-games are meant to be short bursts of gameplay. If we ask the user to wait too long to end up playing a mini-game for only a small period of time anyway, their subjective satisfaction will be lowered, and they will be less eager to play the mini-game in the future.  |
| **Fit Criterion**         | Each mini-game will be benchmarked on a mid-range modern computer by having the mini-game played five times. For each mini-game, for each of those five times, the time elapsed from the main menu until such a time as the game is in a playable state must not be more than 10 seconds.  |
| **Priority**              | High      |

| **ID**: PER-3          | **Type**: Non-functional         |
|-----------------------|--------------------|
| **Related PUC**: N/A        | **Originator**: [Jack](@bucklj4)   |
| **Description**          | For each mini-game that consists of multiple rounds, the time elapsed between one round ending and another beginning must be not be greater than 2 seconds.|
| **Rationale**            | The main components of the mini-game should already be loaded at the conclusion of a round, meaning the loading time between rounds should only be a result of round-specific initialisation.  |
| **Fit Criterion**         | Each mini-game that consists of rounds will be benchmarked on a mid-range modern computer by having each mini-game played five times. For each mini-game, for each of those five times, for each transition between rounds, the time elapsed from the end of one round until such a time as the next round is in a playable state must not be more than 2 seconds.  |
| **Priority**              | High      |

| **ID**: PER-4          | **Type**: Non-functional         |
|-----------------------|--------------------|
| **Related PUC**: N/A        | **Originator**: [Jack](@bucklj4)   |
| **Description**          | Each mini-game must be rendered at a resolution of at least 1280x720, widescreen. All graphics used must appear crisp at this resolution. |
| **Rationale**            | Players expect all modern games to appear in high-definition, and all mid-range modern laptops and monitors are capable of displaying at or greater than this resolution.  |
| **Fit Criterion**         | Each mini-game must be widescreen and run at at least 720p. All non-vector graphics used must be at least 720p.  |
| **Priority**              | Very High      |


#### 3.3.2 Security
| **ID**: <a name="sec-1"></a>SEC-1          | **Type**: Non-functional         |
|-----------------------|--------------------|
| **Related PUC**: N/A        | **Originator**: [Jack](@bucklj4)   |
| **Description**          | Recorded gameplay data which indicates how well a user performed at a motor or cognitive skill must not contain information which can personally identify the player. Such information includes (but is not limited to): name, email, MacID, student number, and telephone number. Moreover, metadata which can be used to personally identify a player cannot be stored. Such information includes (but is not limited to): device IP address, device MAC address, individual identifying browser cookies, and ad tracking identifiers.   |
| **Rationale**            | We want our end-users to feel safe when playing our games. The information we generate from the games can reveal an individual's aptitude at a particular motor or cognitive skill, which might be above or below that of the general population. Such information, if it can be used to personally identify individuals, can be used for prejudice and bias and might pose ethical and legal concerns.    |
| **Fit Criterion**         | Do not ask or attempt to ascertain the information listed above about our players as they play our mini-games.  |
| **Priority**              | High     |

| **ID**: <a name="sec-2"></a>SEC-2          | **Type**: Non-functional         |
|-----------------------|--------------------|
| **Related PUC**: N/A        | **Originator**: [Jack](@bucklj4)   |
| **Description**          | Whenever we require information from the user, we must ask for and accept only the minimal amount of information to perform the task at hand while obeying by [SEC-1](#sec-1). For instance, if we allow users to save game settings and use browser cookies to do so, these cookies must only serve to remember these game settings and cannot personally identify users as described in [SEC-1](#sec-1). Wherever such information can be stored locally on the user's machine to achieve a task at a hand, we must use this option.    |
| **Rationale**            | For ethical reasons, we should always limit the amount of data we collect from individuals.    |
| **Fit Criterion**         | Ask only for the minimal amount of information we need from users and only ask if the task at hand requires *us* to have this data.  |
| **Priority**              | Medium |

| **ID**: SEC-3          | **Type**: Non-functional         |
|-----------------------|--------------------|
| **Related PUC**: N/A        | **Originator**: [Jack](@bucklj4)   |
| **Description**          | Whenever we absolutely require personally identifiable information from the user, contrary to [SEC-1](#sec-1), we must consider this as users participating in a study and must follow relevant and accepted McMaster protocols. In such cases, we must limit the information we obtain to the absolute minimum as per [SEC-2](#sec-2).   |
| **Rationale**            | We need to obey university guidelines.    |
| **Fit Criterion**         | Compliance with university protocols for studies if we require this level of information.  |
| **Priority**              | High |


| **ID**: SEC-4 | **Type**: Non-functional |
|-----------------------|--------------------|
| **Related PUC**: N/A | **Originator**: [Meijing](@lim147) |
| **Description** | The rights to the collected data belong to the players. The rights to interpreted data belongs to the G-ScalE Mini-Game Battery Project and will be stored and used only internally for research purposes. |
| **Rationale** | The collected data is needed to measure the psychomotor abilities. 
| **Fit Criterion** | The data will be only used internally.
| **Priority** | Very High |

#### 3.3.3 Reliability
| **ID**: REL-1          | **Type**: Non-functional         |
|-----------------------|--------------------|
| **Related PUC**: N/A        | **Originator**: [Sasha](@sorainsm)   |
| **Description**          | The cognitive and motor abilities underlying the mini-games must be measured in an objective manner that conforms with existing and accepted best practices for measurement. That is, our measurement techniques for a particular cognitive/motor ability should follow the documented practices of qualified researchers who have performed studies in this area.   |
| **Rationale**            | As computer science students, we are not experienced in the domains of psychology nor kinesiology. As such, as we design our games, we should follow the lead of individuals who are well-versed in these areas to ensure the data we are recording is accurate. Our main focus is thus to "gamify" the practices of these qualified individuals. |
| **Fit Criterion**         | Read domain-level research that pertains to the cognitive and motor abilities we are testing in our games and ensure that our games measure these abilities in a way that conforms to the practices described in this research. |
| **Priority**              | Very high     |

| **ID**: REL-2          | **Type**: Non-functional         |
|-----------------------|--------------------|
| **Related PUC**: N/A        | **Originator**: [Sasha](@sorainsm)   |
| **Description**          | Each game should focus on a limited number of cognitive or motor abilities such that the player's aptitude in such ability can be isolated from that of other abilities.  |
| **Rationale**            | As games become more complex, the user is presented with more gameplay challenges simultaneously, making it harder to clearly measure individual abilities from one another. For this reason, a mini-game should have limited gameplay mechanics and should be designed *around* an ability in mind rather than designing the game first and then trying to add ways to measure an ability.  |
| **Fit Criterion**         | Read domain-level research on a particular cognitive or motor ability, view studies showing how it can be measured, and then develop a game around this such that this ability in particular can be measured in isolation. |
| **Priority**              | Very high     |

| **ID**: REL-3          | **Type**: Non-functional         |
|-----------------------|--------------------|
| **Related PUC**: N/A        | **Originator**: [Jack](@bucklj4)   |
| **Description**          | All games should be play tested so that any potential bugs can be found and fixed.  |
| **Rationale**            | Having bugs in our games will severely lower the subjective satisfaction of our users, making them less likely to keep playing. Even worse, bugs which impede core gameplay mechanics can prove detrimental to the measurements we record for a particular ability.  |
| **Fit Criterion**         | Games appear bug free after being thoroughly play tested. |
| **Priority**              | Very high     |

#### 3.3.4 Availability
| **ID**: AVL-1          | **Type**: Non-functional         |
|-----------------------|--------------------|
| **Related PUC**: N/A        | **Originator**: [Jack](@bucklj4)   |
| **Description**          | The mini-games must be bundled in a collection and must be playable from the same source.   |
| **Rationale**            | We are creating a mini game collection and want to increase the cohesion between the games. Having games spread across different sources would make the user less likely to play all the mini-games as the user is much more likely to try out the different games if the games are bundled as one.  |
| **Fit Criterion**         | Have each mini-game be a component of an overall game collection. |
| **Priority**              | Low     |

| **ID**: AVL-2          | **Type**: Non-functional         |
|-----------------------|--------------------|
| **Related PUC**: N/A        | **Originator**: [Sasha](@sorainsm)/[Jack](@bucklj4)   |
| **Description**          | The mini-games must be playable in a laptop or desktop web browser that supports WebGL. The game must be playable on the latest versions of Windows and macOS using such a browser.   |
| **Rationale**            | Due to COVID-19, getting users to try our games is much harder as we cannot simply put one of our laptops down in front of them with the game already loaded on it. Equally, if we ask users to install our games, they are less likely to do so due to the extra work required. Almost everyone has a laptop or desktop with a browser capable of playing WebGL games. This will enable more users to play our games more quickly and more easily.  |
| **Fit Criterion**         | Design the Unity game with WebGL export in mind. |
| **Priority**              | High     |

| **ID**: AVL-3          | **Type**: Non-functional         |
|-----------------------|--------------------|
| **Related PUC**: N/A        | **Originator**: [Sasha](@sorainsm)   |
| **Description**          | The input controller when playing the mini-games will be limited to a computer keyboard.    |
| **Rationale**            | Due to COVID-19, again we can only reasonably expect that users will have access to a laptop or desktop computer with a web browser, ruling out console controllers and gamepads. We are not considering the mouse as on a laptop, users have trackpads rather than the moveable mice used with desktop computers. Even among desktop mice, designs and ergonomics differ radically, limiting our ability to perform universal mouse measurements.  |
| **Fit Criterion**         | Design games with only the keyboard in mind. |
| **Priority**              | High     |

| **ID**: AVL-4          | **Type**: Non-functional         |
|-----------------------|--------------------|
| **Related PUC**: N/A        | **Originator**: [Sasha](@sorainsm)   |
| **Description**          | Each mini-game must last no longer than a minute.    |
| **Rationale**            | Mini-games are meant to be short, quick bursts of gameplay that you can play when you have a few minutes to spare in your day. Anything longer than a minute is asking too much time from the user.  |
| **Fit Criterion**         | Design games that will always conclude within a minute. |
| **Priority**              | Very high     |

| **ID**: AVL-5          | **Type**: Non-functional         |
|-----------------------|--------------------|
| **Related PUC**: N/A        | **Originator**: [Sasha](@sorainsm)   |
| **Description**          | Each mini-game must contain only simple gameplay mechanics that require few instructions and can be learned quickly.    |
| **Rationale**            | If gameplay mechanics are too complex, the user will be spend more time learning how to play a mini-game than actually playing the mini-game itself, decreasing subjective satisfaction and making it less likely that the player will give the game a chance. |
| **Fit Criterion**         | Design games with simple gameplay mechanics and perform user testing to see if players can readily play the games with little instruction or explanation. |
| **Priority**              | Very high     |


### 3.4 Compliance

| **ID**: COM-1          | **Type**: Non-functional         |
|-----------------------|--------------------|
| **Related PUC**: N/A        | **Originator**: [Sasha](@sorainsm)   |
| **Description**          | Software will released under the MIT license.    |
| **Rationale**            | The MIT license will allow for the use, modification, and distribution of our work. |
| **Fit Criterion**         | Ensure correct [licensing information appears explicitly in our code](https://gitlab.cas.mcmaster.ca/bucklj4/capstone-mini-game-battery-project/-/blob/master/LICENSE) and any assets or resources we use comply with this license. |
| **Priority**              | Low     |

| **ID**: COM-2          | **Type**: Non-functional         |
|-----------------------|--------------------|
| **Related PUC**: N/A        | **Originator**: [Jack](@bucklj4)   |
| **Description**          | A document will be maintained containing the source of every asset used in our work.    |
| **Rationale**            | We will be using a lot of freely available Unity assets. We should keep track of where we obtain assets so we that we have an audit trail if licensing issues arise due to their use. |
| **Fit Criterion**         | Place the asset source in the asset source document whenever a new asset is used in the project. |
| **Priority**              | Low     |

| **ID**: COM-3          | **Type**: Non-functional         |
|-----------------------|--------------------|
| **Related PUC**: N/A        | **Originator**: [Jack](@bucklj4)   |
| **Description**          | A document will be maintained containing domain-level research sources for every measurement technique we use in games.  |
| **Rationale**            | In order to ensure we are measuring data in an accurate way, we'll need to base our measurements off of existing research. When it comes time to do verification, we need to compare how our game measured a certain ability directly against the techniques described in the research, so maintaining this list is essential.  |
| **Fit Criterion**         | Note the research source for the measurement technique of a cognitive or motor ability whenever we begin developing a game around a new ability.  |
| **Priority**              | Very high     |

| **ID**: COM-4          | **Type**: Non-functional         |
|-----------------------|--------------------|
| **Related PUC**: N/A        | **Originator**: [Dr. Carette](@carette)   |
| **Description**          | Maintain a meeting minutes log for each meeting we have with our supervisor.  |
| **Rationale**            | We should be meeting with our supervisor, [Sasha](@sorainsm), every two weeks. To show that we have done this and have discussed meaningful and important topics at such meetings, we should keep track of the main points discussed at a each meeting.   |
| **Fit Criterion**         | Take down notes at each meeting with the supervisor and submit a link to these notes to Avenue.  |
| **Priority**              | Medium    |

| **ID**: COM-5          | **Type**: Non-functional         |
|-----------------------|--------------------|
| **Related PUC**: N/A        | **Originator**: [Jack](@bucklj4)   |
| **Description**          | Maintain an issue board on GitLab that indicates tasks that must be completed, who is assigned to and working on said task, when such task is due, and any comments or concerns that may arise during development on that task.  |
| **Rationale**            | In order to ensure the timely delivery of project milestones, we should be keeping an audit trail of what is left to be done and the progress done towards completing this. Doing this, we will have proof that work on the project has been done *throughout* the academic year and not rushed at the final minute. It also provides our supervisor with an idea about the state of the project and any potential milestone delays and the reasons behind such delays if any arise.     |
| **Fit Criterion**         | Keep the GitLab issue tracker up to date with respect to work done and work to be done.  |
| **Priority**              | High    |



### 3.5 Design and Implementation

#### 3.5.1 Installation
The mini-games will be provided through the web browser, so no installation of software itself is required to run the games.

However, to make sure the games can run stably on a player's web browser, a standard environment has the following requirements:

1. A web server is required to serve the requested pages to the client browser
2. Some necessary add-ons, extensions might need to be enabled.


#### 3.5.2 Distribution
We will host a website through which our Unity games can be played by our players. The games will be distributed primarily via this website, while other export options that Unity supports, such as dedicated game executables, might be used to distribute the game for testing purposes during development.

Any user that visits our website will be able to play our game.


#### 3.5.3 Maintainability
The initial mini-game set will be available on web browsers, where the keyboard is the main interaction tool. The back-end framework will be designed to be platform and interface independent. This controller agnostic framework will make our games easier to maintain as extra controllers can be supported in the future with minimal work needed.

In future versions, games can be extended to enable player interaction through additional interfaces: computer mice, touch pads, and joy-cons.

The framework we build to measure specific cognitive and motor abilities are built with maintainability in mind. For instance, by developing a method to measure token change direction, our work can be used to support  future mini-games apart from Holy Sinkhole that also make use of this ability.

#### 3.5.4 Reusability

The mini games will be designed in a modular way where the framework on interfaces, backend and ability measurement methods will be reused across the whole game design process.

The two groups who are contributing to the Mini-Game Battery Project will collaborate on a backend that can be reused for the creation of additional mini-games in the future. The data collected by our mini games can be used in future cognitive and motor ability testing research and in the development of future versions of the web application.


#### 3.5.5 Portability
The project is platform and interface independent. The web browser games can be run on any mid-range modern computer of all major operating systems.


#### 3.5.6 Cost
There will be no charge for the mini-games.  As the games are designed to help [Sasha](@sorainsm)’s research, we would like more players to get involved and provide rich testing data. For this reason, we cannot charge our players to play the games or we risk shrinking our audience substantially. 

In the game design process, we will use open-source software so there are no firm costs associated with the project. However, if the group decides to allocate money to purchase assets then those will fall into this domain. The paid assets could be: Unity assets, web server costs, as well as other interfaces that end up testing for use in our games, such as joycons, Wii remotes, and XBox controllers.

Labour would be a non-monetary cost. The project is scheduled to take 8 months to complete by five fourth year Computer Science undergraduate students. The expected working time is approximately 8 hours per week.


#### 3.5.7 Project Schedule
| Task  |  Deadline  | Task Description |
| ---- | -------| ----- |
| Problem Statement | 2020-10-02 | A two-page statement of the who/what/when/where of the problem that needs to be solved.|
| Decide the abilities to test and come up with game ideas | 2020-10-08 | Come up with five games aimed to test five different abilities. |
| Requirement Specification | 2020-10-19 | Software Requirements Specification that follows IEEE 29148 guidance|
| Interface of first game| 2020-10-25 | Design and implement the interface of the first mini-game.|
| Backend of first game| 2020-10-26 | Build the backend framework where the game output will be received and automatically stored.|
| First Game Demo | 2020-10-26 | The initial version of the first game should be fully implemented.|
| Menu Page Demo | 2020-10-27 | The initial version of the menu page should be fully implemented.|
| Prototype 1.0 | 2020-10-28 | A 3-min presentation on project overview and a 3-min demo to show a prototype of a web application. |
| Measurement Mechanism for test abilities | 2021-11-05 | Come up with measurement methods for all tested abilities in each game. |
| Design Specification | 2021-11-13| Complete the first version of Design Specification.|
| Fill in audio and additional/extra graphics | TBA | Add additional audio and graphics into the games.|
| Complete all games | TBA | Use the software module of the first game as a template to complete the remaining games. |
| Get the feedback on the game design | TBA | Find peers to play the game and fill in a questionnaire about the interface design, difficulty level, and the interest of gameplay. |
| Prototype 2.0 | TBA | The whole web application should be implemented and the backend framework should be established appropriately. |
| Internal and external testing | TBA | Debugging | 
| Beta/Acceptance testing | TBA | Run the games under real environment before releasing. |
| Initial Mini-Game Release | 2021-02-25 | Release the first version of the web application where the keyboard is the only interaction tool to the games. |
| Collect more data | 2020-03-05 | Ask more people to play the game and collect more data for analysis.|
| Adjust Measurement Mechanism | 2020-03-15 | Adjust the measurement methods based on the collected data.|
| Extend game interface | 2020-03-20 | Upgrade the game interface so that the games can be interacted through more tools. | 
| Final reverse of games | 2020-04-02 | Adjust the games again. | 
| Presentation Slides | 2020-04-03 | Finish the presentation slides and scripts. |
| Faculty of Engineering Capstone Day | 2021-04-05 | Have a showcase for the G-ScalE Mini-Game Battery Project|



#### 3.5.8 Proof of Concept
TODO: add sketches

Prototypes will be used to build initial versions of solutions that implement core functionalities and serve as a proof of concept for verification and possibility of feature extension.



## 4. Verification
##### Interface Verification
For the verification of interface requirements, user surveys would be a relatively effective way. Especially in the user interface related parts, the user's feelings about the content helps us determine whether our requirements are met. Questionnaire surveys are a feasible solution. This can be done when the player finishes playing each mini-game. After enough user feedback is collected, we can summarize this information to determine whether the requirements are satisfied.   

For example, requirement EU-1 describes that "more than 85% of players can master these games through the introduction of rules or simple tutorials." So the questionnaire can ask the player whether he/she feels that it is easy to control the game.     

For hardware and software requirements, user surveys would also be important. Asking players the details of their operating system and PC is helpful for us to measure whether these games meet requirements well. On the other hand, during the development phase, our team will try to use different machines to test the web application and make sure that the requirements are met.

##### Function Verification
For the verification of functional requirements, the verification of the code is the main part of our work. A considerable degree of static testing will be carried out in the initial stage. By analyzing or checking the grammar, structure, process, and interface of the source program, we can have a rough estimate of whether the program completes the results expected by the requirements.

Dynamic testing will also be widely used in the later stage, checking the difference between the actual results and the expected results, and analyzing the performance indicators such as running efficiency, correctness and robustness, so that our code of mini games will be an optimized result.

Unit testing is essential for the analysis of each requirement. This method checks the correctness of these requirements. In this way, the correctness and completeness of each individual requirement can be checked, which partially ensures that the game runs in line with our initial expectations.

Integration testing as well as system testing will also be applied to verify the connection between components and the compliance of the system, to ensure that the web application running properly as a whole. 

Beta/Acceptance testing will be performed after most of our functional requirement testing has been completed, to test for usability, accessibility, understandability requirements, and learning requirements. Such testing will also find remaining bugs or issues specific to a player's environment. This will be done by the other students in the 4ZP6 capstone course.

We will say that a cognitive or motor ability measurement function is correct if the measured results of an individual reflect their aptitude in this ability as measured by an accepted test from a reputable study. We will say that these functions accurately reflect aptitudes if we receive confirmation from [Sasha](@sorainsm) indicating this.


## 5. Cultural Requirements

| ID: CR-1 | Type: Non-Functional |
| ---- | -------|
| PUC: N/A | Originator: [Meijing Li](@lim147) |
| Description | The game must not purposefully offend/discriminate against any cultural or ethnic group. |
| Rationale |Insulting any religious or ethnic group causes conflicts and limits the potential user base. |
| Fit Criterion | Controversial content is not used nor displayed to the players. |
| Priority |High |


## 6. Legal Requirements
### 6.1 Standards Requirements

| ID : ETH-1 | Type : Non-functional Requirements (Ethics) |
| ---- | -------|
| Related PUC: N/A | Originator: [Meijing Li](@lim147) |
| Description | The game will comply with all terms and conditions of McMaster’s Integrity policies and this course as a whole. |
| Rationale | McMaster’s terms and conditions in all these matters need to be followed from a legal standpoint.|
| Fit Criterion | McMaster University’s Academic Integrity regulations will be followed.|
| Priority | Very High |


|ID : STR-1 | Type : Non-functional Requirements (Standards)|
| ---- | -------|
|Related PUC:N/A | Originator: [Meijing Li](@lim147) |
| Description | The game will comply with all standards set out by the course and its professor. |
| Rationale | The university and professor expect these standards to be followed in duration of the capstone course. |
| Fit Criterion | The game will follow standards set by the professor and the teaching assistant. |
| Priority | High |




## 7. Risks
* Time allocation: We might not balance time equally on each portion of the project. As a result, one common problem is that not enough time is allocated to testing the project.
* The amount of collected data: We may not have a great amount of players to play the game, resulting in a lack of data.
* Lack of diverse player backgrounds: Most of our play testers will also be members of our capstone course, which may limit the background profiles and diverseness of the players who are playing our game. This may result in missing data for a particular demographic.
* Ability measurement mechanism: The measurement standards we come up with may not quantify the motor and cognitive abilities very well, which could result in inaccurate testing results. For example, players with different levels of competency do not differ in the testing results.
* The fitness of game and testing abilities: The collected data from the games may not directly reflect the ability aimed to test, or be affected by other factors not accounted for.


## 8. User Documentation And Training
### 8.1 User Documentation Requirements
Documentation will be provided to outline the features, gameplay, game controls and other necessary background information.

### 8.2 Training Requirements
At the beginning of the game, a quick-start tutorial will be provided to illustrate the features and the controls. Other than that, no outside training will be required. 

Additionally, at the beginning of each mini-game the user will be provided with instructions pertaining to how to play the mini-game in question.

## 9. Waiting Room Features
Currently, each game will take no more than 60 seconds to play. With the consideration of the length and difficulty level of each mini-game itself, it may not be necessary to design gameplay that is impossible to achieve by a majority of the players.
* Added game challenges could be adjusted in the process of implementation and testing. 

As the only required interface by the client is the keyboard, due to time constraints mandated, the project deadline, the extended interfaces such as joycons, Wii remotes, and XBox controllers might not be implemented by April 5, 2021.

## 10. Ideas For Solutions
* We have proposed a [set of five mini-games that measure the five abilities we seek](#mini-game-set). We have included in these proposed mini-games gameplay mechanics and added challenges.
* Making use of free resources from Unity asset store to reduce the time spent designing graphics.
* Designing the games in a modular way to reuse the components across the whole development process.


## 11. Appendixes
### 11.1 Identified Cognitive and Motor Abilities
#### 11.1.1 List of Lower Order Cognitive Abilities for Player Model
<table>
<thead>
  <tr>
    <th>Cognitive System</th>
    <th>Subsystem</th>
    <th>Ability</th>
  </tr>
</thead>
<tbody>
  <tr>
    <td rowspan="3">Perception</td>
    <td>Vision</td>
    <td>Facial Recognition<br>Object Recognition<br>Token change direction<br>Type change direction<br>Heading<br>Steering<br>Time to contact</td>
  </tr>
  <tr>
    <td colspan="2">Audial</td>
  </tr>
  <tr>
    <td colspan="2">Tactile</td>
  </tr>
  <tr>
    <td rowspan="2">Attention</td>
    <td>Selective/Focused</td>
    <td>Auditory<br>Visual</td>
  </tr>
  <tr>
    <td colspan="2">Divided</td>
  </tr>
  <tr>
    <td rowspan="3">Memory</td>
    <td>Sensory Store</td>
    <td>Echoic<br>Iconic</td>
  </tr>
  <tr>
    <td>Long Term Memory</td>
    <td>Episodic Memory<br>Semantic Memory<br>Perceptual Representation<br>Procedural Representation</td>
  </tr>
  <tr>
    <td>Working Memory</td>
    <td>Phonological Loop<br>Visuospatial sketchpad<br>Episodic buffer<br>Inhibition<br>Task Shifting<br>Updating working memory<br>Multi-tasking</td>
  </tr>
</tbody>
</table>

#### 11.1.2 Fine motor actions divided by body parts
<table>
<thead>
  <tr>
    <th>Motor System</th>
    <th>Subsystem</th>
    <th>Ability</th>
  </tr>
</thead>
<tbody>
  <tr>
    <td rowspan="3">Hands</td>
    <td>Fingers</td>
    <td>Pressing<br>Swiping<br>Pinching</td>
  </tr>
  <tr>
    <td rowspan="2">Wrist</td>
    <td rowspan="2">Shaking<br>Flicking<br>Pointing<br>Swinging<br>Drawing<br>Tilting</td>
  </tr>
  <tr>
  </tr>
  <tr>
    <td rowspan="2">Head</td>
    <td>Neck</td>
    <td>Moving</td>
  </tr>
  <tr>
    <td>Face</td>
    <td>Speaking<br>Making facial expressions</td>
  </tr>
  <tr>
    <td rowspan="3">Feet</td>
    <td rowspan="3">Ankle and Foot</td>
    <td rowspan="3">Pressing</td>
  </tr>
  <tr>
  </tr>
  <tr>
  </tr>
</tbody>
</table>

#### 11.1.3 Gross motor actions by body part
<table>
<thead>
  <tr>
    <th>Motor System</th>
    <th>Ability</th>
  </tr>
</thead>
<tbody>
  <tr>
    <td rowspan="3">Arms</td>
    <td rowspan="3">Pushing<br>Swinging<br>Drawing<br>Rotating<br>Positioning</td>
  </tr>
  <tr>
  </tr>
  <tr>
  </tr>
  <tr>
    <td rowspan="2">Legs</td>
    <td rowspan="2">Moving<br>Positioning</td>
  </tr>
  <tr>
  </tr>
  <tr>
    <td rowspan="3">Torso</td>
    <td rowspan="3">Positioning</td>
  </tr>
  <tr>
  </tr>
  <tr>
  </tr>
</tbody>
</table>

Soraine, S and Carette, J. (2020) Mechanical Experience, Competency profiles, and Jutsu. Journal of Games, Self, and Society. Vol. 2. o. 150-207
