# Software Requirements Specification
## For Mini-Game Battery Project

Version 0.1  
Prepared by Team Ludus  
Alice Ip - 400078727  
Jack Buckley - 	400144747  
Meijing Li - 400110713  
Shuo He - 400023520  
Yunfei Yang - 400049426  
Instructor: Dr. Jacques Carette  
Course: COMPSCI 4ZP6: Capstone Project  
Date: 2020-10-10   

Table of Contents
=================
* [Revision History](#revision-history)
* 1 [Introduction](#1-introduction)
  * 1.1 [Document Purpose](#11-document-purpose)
  * 1.2 [Stakeholders](#12-stakeholders)
  * 1.3 [Mandated Constraints](#13-Mandated-Constraints)
  * 1.4 [Work Scope](#14-work-scope)
  * 1.5 [Product Scope](#15-product-scope)
  * 1.6 [Definitions, Acronyms and Abbreviations](#16-definitions-acronyms-and-abbreviations)
  * 1.7 [References](#17-references)
  * 1.8 [Document Overview](#18-document-overview)
* 2 [Product Overview](#2-product-overview)
  * 2.1 [Product Perspective](#21-product-perspective)
  * 2.2 [Product Functions](#22-product-functions)
  * 2.3 [Product Constraints](#23-product-constraints)
  * 2.4 [User Characteristics](#24-user-characteristics)
  * 2.5 [Assumptions and Dependencies](#25-assumptions-and-dependencies)
  * 2.6 [Apportioning of Requirements](#26-apportioning-of-requirements)
* 3 [Requirements](#3-requirements)
  * 3.1 [External Interfaces](#31-external-interfaces) 
    * 3.1.1 [User Interfaces](#311-user-interfaces)
    * 3.1.2 [Hardware Interfaces](#312-hardware-interfaces)
    * 3.1.3 [Software Interfaces](#313-software-interfaces)
  * 3.2 [Functional](#32-functional) 
  * 3.3 [Quality of Service](#33-quality-of-service)
    * 3.3.1 [Performance](#331-performance)
    * 3.3.2 [Security](#332-security)
    * 3.3.3 [Reliability](#333-reliability)
    * 3.3.4 [Availability](#334-availability)
  * 3.4 [Compliance](#34-compliance)
  * 3.5 [Design and Implementation](#35-design-and-implementation) 
    * 3.5.1 [Installation](#351-installation)
    * 3.5.2 [Distribution](#352-distribution)
    * 3.5.3 [Maintainability](#353-maintainability)
    * 3.5.4 [Reusability](#354-reusability)
    * 3.5.5 [Portability](#355-portability)
    * 3.5.6 [Cost](#356-cost)
    * 3.5.7 [Deadline](#357-deadline)
    * 3.5.8 [Proof of Concept](#358-proof-of-concept)
* 4 [Verification](#4-verification) 
* 5 [Appendixes](#5-appendixes)

## Revision History
| Name | Date    | Reason For Changes  | Version   |  
| ---- | ------- | ------------------- | --------- |  
|      |         |                     |           |  
|      |         |                     |           |  
|      |         |                     |           |  

## 1. Introduction

### 1.1 Document Purpose
##### 1.1.a Background 
The G-ScalE Mini-Game Battery Project is a capstone project for COMPSCI 4ZP6 at McMaster University. This project is developed by Team Ludus, a team that consists of five fourth-year Computer Science students from McMaster University under the supervision of Sasha Soraine. Completion of this capstone project is a mandatory requirement for graduation and also a process for students to apply their knowledge in a year long project.

Our capstone group is developing the G-ScalE Mini-Game Battery Project, a project composed of five to ten single-player mini games designed in Unity, to measure approximately five out of 46 identified human cognitive and psychomotor skills. Each game will take players no more than a minute to play, and will be used to establish player competencies in a specific cognitive or psychomotor group. The structure of the games will be similar to minigame compilations, like Mario Party.

##### 1.1.b Goals
The purpose of the capstone project is to give students an opportunity to apply and practice their knowledge learned through their university study experience to create a set of mini-games that could practice their creativity, teamwork, and technical skills. This will also let students participate in a full life-cycle of software development, which enhances the understanding of the software design process. 

The goal of the G-ScalE Mini-Game Battery Project is to measure a subset of cognitive and motor abilities through the process of playing and enjoying the games.

##### 1.1.c Purpose
The purpose of this requirements document is to lay out who the stakeholders of this project are, the constraints and scope of the project, as well as the functional and non-functional requirements that need to be met to ensure the success of the project. In this way, this document serves as a contract between us, Team Ludus, and our stakeholders on what we will deliver to ensure our solution fully addresses the problem our stakeholders face, as laid out in our previous project summary document, all while ensuring we meet the many individual requirements that compose this problem.

### 1.2 Stakeholders
#### The Client
* Dr. Jacques Carette: The professor for the Computer Science Capstone Project: COMPSCI 4ZP6. He establishes the requirements, milestones, deadlines, and other course-related objectives. He is also responsible for evaluating and grading the final project.

* Sasha Soraine: A PhD. Candidate in the Department of Computing and Software at McMaster University. She is our supervisor for the G-ScalE Mini-Game Battery Project. She will provide us with detailed information and requirements about the project and assist us in finishing the challenges during the development process.


* Ethan Chan and Brendan Fallon: These are the TAs for COMPSCI 4ZP6. They will be responsible for grading and providing feedback on project deliverables.


#### The User Group
* Players: The people who will be playing the mini-games, consisting of people of different ages. They will participate in gameplay designed to provide data that we will measure to extract cognitive and motor ability aptitudes.

* Other students taking COMPSCI 4ZP6: The other students taking the capstone course will submit feedback and bug reports during the testing phase of the project.

#### Others
* Team Ludus: The developers of the G-ScalE Mini-Game Battery Project. This team consists of five students enrolled in the Computer Science Capstone Project: COMPSCI 4ZP6. The final deliverable and the documents will be made by these team members who will also give a demo presentation of the project at the end of the semester to a panel of judges at the Faculty of Engineering Capstone Day.

* Team Mactivision: The other team working under our supervisor to develop a different set of mini-games for the G-ScalE Mini-Game Battery Project. The members of this group include: Bryan Chiu, Sijie Zhou, Kunyuan Cao, David Hospital, and Mike Tee. We will be collaborating with this group at a further stage of development in the project.

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
* All course deliverables must be completed on time according to the schedule provided by Dr. Carette. The final project must be fully finished by April 5th.

##### Budget Constraints
* There are no budget constraints for this project. For the purposes of our project, we will not be paid nor will we need to pay for any software.

##### Final Product Constraints
* The final product must consist of several one-minute mini-games that will measure at least five cognitive and motor abilities from the set of identified cognitive and motor abilities.

### 1.4 Work Scope
##### Existing Inspirations
* Super Mario Party (Nintendo, 2018):  A party video game where players compete with each other while having fun by playing several small games to earn points. Unique challenges are designed in those small games that require different abilities to be successful. Games in the G-ScalE Mini-Game Battery Project will be designed similarly to the mini-games found in Super Mario Party.

##### Context of the Work
* The completion of this project is the focus of the course COMPSCI 4ZP6 and is a key requirement needed to graduate from the undergraduate computer science program at McMaster. 
* Beyond the end of the course, future work on the project could entail the development of further mini-games testing other abilities as well as the use of other input controllers apart from the keyboard.

### 1.5 Product Scope
##### Product Boundary
* The final product will be a series of mini-games consisting of at least five selected cognitive and motor abilities to be measured.
* The product will not be commercially sold and the platform will solely be laptop and desktop computers.
* Each mini-game is independent and has no continuity; players can choose which mini-game to start from freely.

##### Mini-Game Set
<TO DO>

1. Meijing [name]: A card game where 9 cards(3 x 3) face down on the screen. Each card has one of three kind of objects. The image on the card of the same kind could vary but they are related. When the player clicks on the card, the card flips over and the player can see what is the object on the card. When flipping over the second card, if the object belongs to the same kind of the first card, the both cards remain face up, otherwise both cards face down. Same for the third card. When the players have three cards in the same object all face up, this object is cleared. To win the game, the player must clear all three objects.


2. [Jack] Sinkhole game: The user has to move out of the way of an impending sinkhole by moving left or right. They can see that a sinkhole is about to appear and have to move left or right to avoid.


3. [Yunfei] [name]: Row of lights that light up sequentially, and you must press a button when the light reaches green (For example)


4. [Shuo] [name]: a piano game where each key represents a musical note and the player is asked to listen to a section of music and duplicate that through pressing the keyboard. 


5. [Alice] [name]: One object will be displayed, and then it will be put in a cup with two other identical cups. The three cups will be swapped, the player will need to find the correct cup with that object. 




##### Product Use Case (PUC) Table  
| PUC | PUC Name | Actor(s) | Input/Output |  
| ---- | ------- | ----------| --------- | 
| 1 | Choose Game | Player | Key Input(In), Intial Game Data (Out)  |  
| 2 | Exit Game | Player | Key Input(In) |  
| 3 | Pause Game | Player | Key Input(In), Game Status(Out) |  
| 4 | Resume Game | Player | Key Input(In), Game Status(Out) |  
| 5 | End Game | Player | Key Input(In), Game Status(Out) |   
| 5 | Finish Game | Player | Key Input(In), Game Status(Out) |   
| 6 | Check Final Score | Player | Key Input(In), Score Data(Out) |  
| 7 | Pick Cards | Player |Key Input(In), Player Game Data (Out) |  
| 8 | Move Character | Player | Key Input(In), Character Position(Out) |  
| 9 | Collect Items | Player | Key Input(In), Player Game Data(Out) |  
| 10 | Play the Piano | Player | Key Input(In), Player Game Data(Out), Auditory Data(Out) |  
| 11 | Click the Item | Player | Key Input(In), Player Game Data(Out) |  
| 12 | Select the Ball | Player | Key Input(In), Player Game Data(Out)|  
| 13 | Click Words | Player | Key Input(In), Player Game Data(Out) |  
| 14 | Select Cup | Player | Key Input(In), Player Game Data(Out) |

##### Individual Product Use Cases
| PUC No.1 | Event: Choose Game |  
| ---- | ------- | 
| Trigger | Player selects a mini-game from the game menu |  
| Preconditions | Game status is null or end, and in the main menu |  
| Procedure | 1.Initialize new game data. 2.Load new game data 3.Game status is switched to active|  
| Outcome | Player is in a mini-game instance and ready to play |  

| PUC No.2 | Event: Exit Game |  
| ---- | ------- | 
| Trigger | Player selects "Exit" option |  
| Preconditions |Game status is null or end, and in the main menu |
| Procedure | 1.Display message "Do you really want to exit?". 2.Get input from (1). 3.Close the tab of the web application depending on the input|  
| Outcome | Player exits the web application |  

| PUC No.3 | Event: Pause Game | 
| ---- | ------- | 
| Trigger | Player selects "Pause" option |  
| Preconditions | Player is in a mini-game, and the game status is active |
| Procedure | 1.Game is switched into pause status. 2.A Pause menu is displayed |  
| Outcome | Game is paused and a pause menu is on the screen |  

| PUC No.4 | Event: Resume Game | 
| ---- | ------- | 
| Trigger | Player selects "Resume" option from pause menu |  
| Preconditions | Player is in a mini-game, and the game status is paused |  
| Procedure | 1.Game is switched into active status. 2.Pause menu is not displayed |    
| Outcome | Game is resumed and players can play the game |  

| PUC No.5 | Event: End Game |  
| ---- | ------- | 
| Trigger | Player selects "End" option from pause menu |  
| Preconditions | Player is in a mini-game, and the game status is paused |
| Procedure | 1.Game is switched into finished status. 2.End screen is displayed |   
| Outcome | Game is terminated and the end screen is on screen |  

| PUC No.6 | Event: Finish Game |  
| ---- | ------- | 
| Trigger | Players finish the game |  
| Preconditions | Player is in a mini-game, and the game status is active |
| Procedure | 1.Game is switched into "finished" status. 2.End screen is displayed |    
| Outcome | Game is terminated and the end screen is on screen |  

| PUC No.6 | Event: Check Final Score |  
| ---- | ------- | 
| Trigger | The end menu is displayed |  
| Preconditions | Game status is "finished", and the end menu is displayed |
| Procedure | 1.The final score will be displayed |    
| Outcome | The final score of the player is displayed |  

| PUC No.7 | Event: Flip Card | 
| ---- | ------- | 
| Trigger | Players press the key for certain card  |  
| Preconditions | Current game is (), and the game status is active |
| Procedure | 1.Display the flipped card. 2. Determine if there is another correct card flipped  |    
| Outcome | Player gets score if the correct card is flipped, otherwise the flipped card will be unflipped |  

| PUC No.8 | Event: Move Character | 
| ---- | ------- | 
| Trigger | Player presses the key to move the character  |  
| Preconditions | Current game is (), and the game status is active |
| Procedure | 1.Character moves according to the pressed key |    
| Outcome | Character's position in 2-dimension changed according to the key until the key is released | 

| PUC No.9 | Event: Collect items | 
| ---- | ------- | 
| Trigger | Character is under the coins that are falling down  |  
| Preconditions | Current game is (), and the game status is active |
| Procedure | 1.The item collected is displayed. 2. Determine if the item is coin or enemy |    
| Outcome | Player gets score depending on the result | 

| PUC No.10 | Event: Play the Piano | 
| ---- | ------- | 
| Trigger | Player presses the key for a certain musical note  |  
| Preconditions | Current game is (), and the game status is active |
| Procedure | 1.The sound of the pressed musical note is played. 2.Determine if the musical note is correct |    
| Outcome | Player gets score depending on the result | 

| PUC No.11 | Event: Click the Item | 
| ---- | ------- | 
| Trigger | Player presses the key for certain Item |  
| Preconditions | Current game is (), and the game status is active |
| Procedure | 1.Record the time between from the appearance of the item to the click event |    
| Outcome | Player gets score depending on the result | 

TODO: PUC 12,13,14








(Identify the product whose software requirements are specified in this document, including the revision or release number. Explain what the product that is covered by this SRS will do, particularly if this SRS describes only part of the system or a single subsystem. Provide a short description of the software being specified and its purpose, including relevant benefits, objectives, and goals. Relate the software to corporate goals or business strategies. If a separate vision and scope document is available, refer to it rather than duplicating its contents here.)


### 1.6 Definitions, Acronyms and Abbreviations
* Unity: A cross-platform game engine developed by Unity Technologies.

* UI: User Interface. The interface through which users interact with the hardware and software of computers and other electronic devices.
* Cognitive abilities: Brain-based skills which are needed in acquisition of knowledge, manipulation of information, and reasoning.
* Motor abilities: Learned abilities to cause a predetermined movement outcome with maximum certainty.
* Parameters: A numerical value or property of the software that helps determine the environment or state of the software.  
* PC: Personal Computer. A multi-purpose computer whose size, capabilities, and price make it feasible for individual use.
* OS: Operating System. The system software that manages computer hardware, software resources, and provides common services for computer programs.




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
Our product is composed of a series of small video games that focuses on five select cognitive abilities out of 46 identified human cognitive and psychomotor requirements associated with video game challenges: object recognition, token change direction, time to contact, audial perception, and selective visual. Each individual game will focus primarily on one of the five selected abilities. The games will be accessible by the players through a web application that runs on a web server, and collects input through the keyboard and mouse. The input collected by the web application will be used to establish an ability baseline for the player, and to determine how parameter changes in the atomic challenges affect mechanical achievability and mechanical difficulty. The research results from the product will be used to contribute to the creation of innovative approaches in future game development and will bring about a greater understanding of the underlying science behind video games.

### 2.1 Product Perspective
The G-ScalE project has existing research by Sasha Soraine, who has created a framework for identifying atomic challenges in video games and hypotheses about the mechanical experiences of video games.  Our capstone group is one of two groups that will be working on products to test and further the hypotheses and research already done. The two groups will be sharing the backend framework and will design and implement it such that the video game designs from both groups adhere to it.  The video game designs will reference existing test methods and research of the selected cognitive abilities to design video games that can be used to test and capture data related to the cognitive abilities.



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
* Player is a generic able-bodied neurotypical person

(Identify the various user classes that you anticipate will use this product. User classes may be differentiated based on frequency of use, subset of product functions used, technical expertise, security or privilege levels, educational level, or experience. Describe the pertinent characteristics of each user class. Certain requirements may pertain only to certain user classes. Distinguish the most important user classes for this product from those who are less important to satisfy.)

### 2.5 Assumptions and Dependencies

* Free-to-use assets from the Unity asset store will remain free to use during project implementation and after project completion
* The license requirements for the software being developed and Unity do not change
* The expectations required by the project lead do not change
* The product will not be commercially released
* Game assets may come from third party resources
* There exist effective methods to measure a subset of the cognitive and motor abilities using the keyboard only as the main input device

### 2.6 Apportioning of Requirements
SKIP FOR NOW
(Apportion the software requirements to software elements. For requirements that will require implementation over multiple software elements, or when allocation to a software element is initially undefined, this should be so stated. A cross reference table by function and software element should be used to summarize the apportioning.

Identify requirements that may be delayed until future versions of the system (e.g., blocks and/or increments).)

## 3. Requirements
> This section specifies the software product's requirements. Specify all of the software requirements to a level of detail sufficient to enable designers to design a software system to satisfy those requirements, and to enable testers to test that the software system satisfies those requirements.

> The specific requirements should:
* Be uniquely identifiable.
* State the subject of the requirement (e.g., system, software, etc.) and what shall be done.
* Optionally state the conditions and constraints, if any.
* Describe every input (stimulus) into the software system, every output (response) from the software system, and all functions performed by the software system in response to an input or in support of an output.
* Be verifiable (e.g., the requirement realization can be proven to the customer's satisfaction)
* Conform to agreed upon syntax, keywords, and terms.

### 3.1 External Interfaces

#### 3.1.1 User interfaces
Define the software components for which a user interface is needed. Describe the logical characteristics of each interface between the software product and the users. This may include sample screen images, any GUI standards or product family style guides that are to be followed, screen layout constraints, standard buttons and functions (e.g., help) that will appear on every screen, keyboard shortcuts, error message display standards, and so on. Details of the user interface design should be documented in a separate user interface specification.

Could be further divided into Usability and Convenience requirements.
        
##### Ease of Use Requirements   
| **ID**:  EU-1        | **Type**: Non-Functional (Ease of Use)  |      
| -------------------- | --------------------------------------- |      
| **Related PUC**: N/A | **Originator**: Yunfei                  |      
| **Description**      | The player should know how to control all the mini games after a relatively short game time.                      |    
| **Rationale**        | The control of mini games should not make players confused or frustrated.                                  |    
| **Fit Criterion**    | More than 85% of players can master these games through the introduction of rules or simple tutorials.           |   
| **Priority**         | Medium                              |    

| **ID**:  EU-2        | **Type**: Non-Functional (Ease of Use)  |      
| -------------------- | --------------------------------------- |      
| **Related PUC**: N/A | **Originator**: Yunfei                  |      
| **Description**      | Players from all ages can enjoy playing these games. |  
| **Rationale**        | Collecting game data from different age groups is very important for measuring motor and cognitive abilities of different individuals.                                             |    
| **Fit Criterion**    | General population of players aged about 10 to 70 can all play these games. |   
| **Priority**         | High                                |   
      
##### Personalization Requirements     
| **ID**:  PR-1        | **Type**: Non-Functional (Personalization)  |      
| -------------------- | ------------------------------------------- |      
| **Related PUC**: N/A | **Originator**: Yunfei                      |      
| **Description**      | In each mini game, players can modify the game settings to adapt to their needs for volume and window size.      |    
| **Rationale**        | Each player has their preference on setting options. |     
| **Fit Criterion**    | Setting options can be adjusted.        |   
| **Priority**         | Low                                     |    
       
##### Learning Requirements  
| **ID**:  LR-1        | **Type**: Non-Functional (Learning)      |      
| -------------------- | ---------------------------------------- |      
| **Related PUC**: N/A | **Originator**: Yunfei                   |      
| **Description**      | Players can learn these games quickly, making the motor and cognitive ability data obtained accurate.   |    
| **Rationale**        | The data about motor and cognitive abilities should not be made inaccurate because the player does not understand the game. |    
| **Fit Criterion**    | More than 85% of players can master these games through the introduction of rules or simple tutorials.        |   
| **Priority**         | Medium                               |    
       
##### Understandability and Politeness Requirements  
| **ID**:  UP-1        | **Type**: Non-Functional (Understandability and Politeness)      |      
| -------------------- | ---------------------------------------- |      
| **Related PUC**: N/A | **Originator**: Yunfei                   |      
| **Description**      | All text related to settings and rules that appear in mini games must be in English.   |    
| **Rationale**        | The target players of these mini games are in English-speaking countries, so the language of games should be in English. |    
| **Fit Criterion**    | All text related to settings and rules that appear in mini games should be written in English. |   
| **Priority**         | High                               |   

| **ID**:  UP-2        | **Type**: Non-Functional (Understandability and Politeness)      |      
| -------------------- | ---------------------------------------- |      
| **Related PUC**: N/A | **Originator**: Yunfei                   |      
| **Description**      | All symbols and icons that appear in mini games should be easy to understand                                      |    
| **Rationale**        | The gameplay should not be influenced by complicated and obscure symbols.                                  |    
| **Fit Criterion**    | All symbols and icons that appear in mini games should convey intuitive information.                               |   
| **Priority**         | Medium                                   |  

##### Accessibility Requirements
| **ID**:  AR-1        | **Type**: Non-Functional (Accessibility) |    
| -------------------- | ---------------------------------------- |      
| **Related PUC**: N/A | **Originator**: Yunfei                   |      
| **Description**      | Some games that have special requirements for players need to be noted.   |    
| **Rationale**        | Theoretically, mini games should be suitable for all people from ages 10 to 70. However, for some people with disabilities, there are still games that cannot be played, which require explanation.   |    
| **Fit Criterion**    | Games about colors need to explain that color-blind players cannot participate. Games about audial perception need to explain that hearing impaired players cannot participate. Certain games about motor abilities need to explain that different kinds of disabled people cannot participate. |   
| **Priority**         | High                               |    


#### 3.1.2 Hardware interfaces
Describe the logical and physical characteristics of each interface between the software product and the hardware components of the system. This may include the supported device types, the nature of the data and control interactions between the software and the hardware, and communication protocols to be used.

* All the games created during this project using Windows system.  

mid-range Modern computer: e.g. computers no later than 2012.
CPU capacity
Resolution
RAM, storage, memory
Processor
The keyboard in good conditions: no lags, no issue on pressing
Internet speed

#### 3.1.3 Software interfaces
Describe the connections between this product and other specific software components (name and version), including databases, operating systems, tools, libraries, and integrated commercial components. Identify the data items or messages coming into the system and going out and describe the purpose of each. Describe the services needed and the nature of communications. Refer to documents that describe detailed application programming interface protocols. Identify data that will be shared across software components. If the data sharing mechanism must be implemented in a specific way (for example, use of a global data area in a multitasking operating system), specify this as an implementation constraint.

* Unity must be used to create mini games. No other game engine or library will be used.

### 3.2 Functional
> This section specifies the requirements of functional effects that the software-to-be is to have on its environment.

##### Core Mechanics


### 3.3 Quality of Service
> This section states additional, quality-related property requirements that the functional effects of the software should present.

#### 3.3.1 Performance
| **ID**: PER-1          | **Type**: Non-functional         |
|-----------------------|--------------------|
| **Related PUC**: TODO        | **Originator**: Jack   |
| **Description**          | Each mini-game must maintain a frame rate of at least 30 frames per second on a mid-range modern computer.|
| **Rationale**            | In order for games, especially mini-games, to feel interactive they have to maintain a fluid frame rate. In commercial games, 30 frames per second is considered the minimum acceptable frame rate for playability. Resource: https://www.logicalincrements.com/articles/framerate   |
| **Fit Criterion**         | Each mini-game will be benchmarked on a mid-range modern computer by having the mini-game played five times. For each mini-game, for each of those five times, the monitored frame rate must not drop below 30 frames per second.  |
| **Priority**              | Very High      |

| **ID**: PER-2          | **Type**: Non-functional         |
|-----------------------|--------------------|
| **Related PUC**: TODO        | **Originator**: Jack   |
| **Description**          | Each mini-game must load to a playable state within 10 seconds from the time the user indicates which game they wish to play on the main menu.|
| **Rationale**            | Mini-games are meant to be short bursts of gameplay. If we ask the user to wait too long to end up playing a mini-game for only a small period of time anyway, their subjective satisfaction will be lowered, and they will be less eager to play the mini-game in the future.  |
| **Fit Criterion**         | Each mini-game will be benchmarked on a mid-range modern computer by having the mini-game played five times. For each mini-game, for each of those five times, the time elapsed from the main menu until such a time as the game is in a playable state must not be more than 10 seconds.  |
| **Priority**              | High      |

| **ID**: PER-3          | **Type**: Non-functional         |
|-----------------------|--------------------|
| **Related PUC**: TODO        | **Originator**: Jack   |
| **Description**          | For each mini-game that consists of multiple rounds, the time elapsed between one round ending and another beginning must be not be greater than 2 seconds.|
| **Rationale**            | The main components of the mini-game should already be loaded at the conclusion of a round, meaning the loading time between rounds should only be a result of round-specific initialisation.  |
| **Fit Criterion**         | Each mini-game that consists of rounds will be benchmarked on a mid-range modern computer by having each mini-game played five times. For each mini-game, for each of those five times, for each transition between rounds, the time elapsed from the end of one round until such a time as the next round is in a playable state must not be more than 2 seconds.  |
| **Priority**              | High      |

| **ID**: PER-4          | **Type**: Non-functional         |
|-----------------------|--------------------|
| **Related PUC**: TODO        | **Originator**: Jack   |
| **Description**          | Each mini-game must be rendered at a resolution of at least 1280x720, widescreen. All graphics used must appear crisp at this resolution. |
| **Rationale**            | Players expect all modern games to appear in high-definition, and all mid-range modern laptops and monitors are capable of displaying at or greater than this resolution.  |
| **Fit Criterion**         | Each mini-game must be widescreen and run at at least 720p. All non-vector graphics used must be at least 720p.  |
| **Priority**              | Very High      |




If there are performance requirements for the product under various circumstances, state them here and explain their rationale, to help the developers understand the intent and make suitable design choices. Specify the timing relationships for real time systems. Make such requirements as specific as possible. You may need to state performance requirements for individual functional requirements or features.

#### 3.3.2 Security
| **ID**: SEC-1          | **Type**: Non-functional         |
|-----------------------|--------------------|
| **Related PUC**: TODO        | **Originator**: Jack   |
| **Description**          | Recorded gameplay data which indicates how well a user performed at a motor or cognitive skill must not contain information which can personally identify the player. Such information includes (but is not limited to): name, email, MacID, student number, and telephone number. Moreover, metadata which can be used to personally identify a player cannot be stored. Such information includes (but is not limited to): device IP address, device MAC address, individual identifying browser cookies, and ad tracking identifiers.   |
| **Rationale**            | We want our end-users to feel safe when playing our games. The information we generate from the games can reveal an individual's aptitude at a particular motor or cognitive skill, which might be above or below that of the general population. Such information, if it can be used to personally identify individuals, can be used for prejudice and bias and might pose ethical and legal concerns.    |
| **Fit Criterion**         | Do not ask or attempt to ascertain the information listed above out of our players as they play our mini-games.  |
| **Priority**              | High     |

| **ID**: SEC-2          | **Type**: Non-functional         |
|-----------------------|--------------------|
| **Related PUC**: TODO        | **Originator**: Jack   |
| **Description**          | Whenever we require information from the user, we must ask for and accept only the minimal amount of information to perform the task at hand while obeying by SEC-1. For instance, if we allow users to save game settings and use browser cookies to do so, these cookies must only serve to remember these game settings and cannot personally identify users as described in SEC-1. Wherever such information can be stored locally on the user's machine to achieve a task at a hand, we must use this option.    |
| **Rationale**            | For ethical reasons, we should always limit the amount of data we collect from individuals.    |
| **Fit Criterion**         | Ask only for the minimal amount of information we need from users and only ask if the task at hand requires *us* to have this data.  |
| **Priority**              | Medium |

| **ID**: SEC-3          | **Type**: Non-functional         |
|-----------------------|--------------------|
| **Related PUC**: TODO        | **Originator**: Jack   |
| **Description**          | Whenever we absolutely require personally identifiable information from the user, contrary to SEC-1, we must consider this as users participating in a study and must follow relevant and accepted McMaster protocols. In such cases, we must limit the information we obtain to the absolute minimum as per SEC-2.   |
| **Rationale**            | We need to obey university guidelines.    |
| **Fit Criterion**         | Compliance with university protocols for studies if we require this level of information  |
| **Priority**              | High |



|ID: SEC-4 | Type: Non-functional Requirements |
| Related PUC: N/A | Originator: Meijing |
| Description | The right of the collected data belongs to the players. The right of interpreted data belongs to the Mini-Game Battery Project and will be stored and used only internally for the research purpose. |
| Rationale| The collected data is needed to measure the psychomotor abilities. 
| Fit Criterion | The data will be only used internally.
| Priority | Very High |



#### 3.3.3 Reliability
| **ID**: REL-1          | **Type**: Non-functional         |
|-----------------------|--------------------|
| **Related PUC**: TODO        | **Originator**: Sasha   |
| **Description**          | The cognitive and motor abilities underlying the mini-games must be measured in an objective manner that conforms with existing and accepted best practices for measurement. That is, our measurement techniques for a particular cognitive/motor ability should follow the documented practices of qualified researchers who have performed studies in this area.   |
| **Rationale**            | As computer science students, we are not experienced in the domains of psychology nor kinesiology. As such, as we design our games, we should follow the lead of individuals who are well-versed in these areas to ensure the data we are recording is accurate. Our main focus is thus to "gamify" the practices of these qualified individuals. |
| **Fit Criterion**         | Read domain-level research that pertains to the cognitive and motor abilities we are testing in our games and ensure that our games measure these abilities in a way that conforms to the practices described in this research. |
| **Priority**              | Very high     |

| **ID**: REL-2          | **Type**: Non-functional         |
|-----------------------|--------------------|
| **Related PUC**: TODO        | **Originator**: Sasha   |
| **Description**          | Each game should focus on a limited number of cognitive or motor abilities such that the player's aptitude in such ability can be isolated from that of other abilities.  |
| **Rationale**            | As games become more complex, the user is presented with more gameplay challenges simultaneously, making it harder to clearly measure individual abilities from one another. For this reason, a mini-game should have limited gameplay mechanics and should be designed *around* an ability in mind rather than designing the game first and then trying to add ways to measure an ability.  |
| **Fit Criterion**         | Read domain-level research on a particular cognitive or motor ability, view studies showing how it can be measured, and then develop a game around this such that this ability in particular can be measured in isolation. |
| **Priority**              | Very high     |

| **ID**: REL-3          | **Type**: Non-functional         |
|-----------------------|--------------------|
| **Related PUC**: TODO        | **Originator**: Jack   |
| **Description**          | All games should be play tested so that any potential bugs can be found and fixed.  |
| **Rationale**            | Having bugs in our games will severely lower the subjective satisfaction of our users, making them less likely to keep playing. Even worse, bugs which impede core gameplay mechanics can prove detrimental to the measurements we record for a particular ability.  |
| **Fit Criterion**         | Games appear bug free after being thoroughly play tested. |
| **Priority**              | Very high     |

Specify the factors required to establish the required reliability of the software system at time of delivery.

#### 3.3.4 Availability
| **ID**: AVL-1          | **Type**: Non-functional         |
|-----------------------|--------------------|
| **Related PUC**: TODO        | **Originator**: Jack   |
| **Description**          | The mini-games must be bundled in a collection and must be playable from the same source.   |
| **Rationale**            | We are creating a mini game collection and want to increase the cohesion between the games. Having games spread across different sources would make the user less likely to play all the mini-games as the user is much more likely to try out the different games if the games are bundled as one.  |
| **Fit Criterion**         | Have each mini-game be a component of an overall game collection. |
| **Priority**              | Low     |

| **ID**: AVL-2          | **Type**: Non-functional         |
|-----------------------|--------------------|
| **Related PUC**: TODO        | **Originator**: Sasha/Jack   |
| **Description**          | The mini-games must be playable in a laptop or desktop web browser that supports WebGL. The game must be playable on the latest versions of Windows and macOS using such a browser.   |
| **Rationale**            | Due to COVID-19, getting users to try our games is much harder as we cannot simply put one of our laptops down in front of them with the game already loaded on it. Equally, if we ask users to install our games, they are less likely to do so due to the extra work required. Almost everyone has a laptop or desktop with a browser capable of playing WebGL games. This will enable more users to play our games more quickly and more easily.  |
| **Fit Criterion**         | Design the Unity game with WebGL export in mind. |
| **Priority**              | High     |

| **ID**: AVL-3          | **Type**: Non-functional         |
|-----------------------|--------------------|
| **Related PUC**: TODO        | **Originator**: Sasha   |
| **Description**          | The input controller when playing the mini-games will be limited to a computer keyboard.    |
| **Rationale**            | Due to COVID-19, again we can only reasonably expect that users will have access to a laptop or desktop computer with a web browser, ruling out console controllers and gamepads. We are not considering the mouse as on a laptop, users have trackpads rather than the moveable mice used with desktop computers. Even among desktop mice, designs and ergonomics differ radically, limiting our ability to perform universal mouse measurements.  |
| **Fit Criterion**         | Design games with only the keyboard in mind. |
| **Priority**              | High     |

| **ID**: AVL-4          | **Type**: Non-functional         |
|-----------------------|--------------------|
| **Related PUC**: TODO        | **Originator**: Sasha   |
| **Description**          | Each mini-game must last no longer than a minute.    |
| **Rationale**            | Mini-games are meant to be short, quick bursts of gameplay that you can play when you have a few minutes to spare in your day. Anything longer than a minute is asking too much time from the user.  |
| **Fit Criterion**         | Design games that will always conclude within a minute. |
| **Priority**              | Very high     |

| **ID**: AVL-5          | **Type**: Non-functional         |
|-----------------------|--------------------|
| **Related PUC**: TODO        | **Originator**: Sasha   |
| **Description**          | Each mini-game must contain only simple gameplay mechanics that require few instructions and can be learned quickly.    |
| **Rationale**            | If gameplay mechanics are too complex, the user will be spend more time learning how to play a mini-game than actually playing the mini-game itself, decreasing subjective satisfaction and making it less likely that the player will give the game a chance. |
| **Fit Criterion**         | Design games with simple gameplay mechanics and perform user testing to see if players can readily play the games with little instruction or explanation. |
| **Priority**              | Very high     |


Specify the factors required to guarantee a defined availability level for the entire system such as checkpoint, recovery, and restart.

### 3.4 Compliance

| **ID**: COM-1          | **Type**: Non-functional         |
|-----------------------|--------------------|
| **Related PUC**: TODO        | **Originator**: Sasha   |
| **Description**          | Software will released under the MIT license.    |
| **Rationale**            | The MIT license will allow for the use, modification, and distribution of our work. |
| **Fit Criterion**         | Ensure correct licensing information appears explicitly in our code and any assets or resources we use comply with this license. |
| **Priority**              | Low     |

| **ID**: COM-2          | **Type**: Non-functional         |
|-----------------------|--------------------|
| **Related PUC**: TODO        | **Originator**: Jack   |
| **Description**          | A document will be maintained containing the source of every asset used in our work.    |
| **Rationale**            | We will be using a lot of freely available Unity assets. We should keep track of where we obtain assets so we that we have an audit trail if licensing issues arise due to their use. |
| **Fit Criterion**         | Place the asset source in the asset source document whenever a new asset is used in the project. |
| **Priority**              | Low     |

| **ID**: COM-3          | **Type**: Non-functional         |
|-----------------------|--------------------|
| **Related PUC**: TODO        | **Originator**: Jack   |
| **Description**          | A document will be maintained containing domain-level research sources for every measurement technique we use in games.  |
| **Rationale**            | In order to ensure we are measuring data in an accurate way, we'll need to base our measurements off of existing research. When it comes time to do verification, we need to compare how our game measured a certain ability directly against the techniques described in the research, so maintaining this list is essential.  |
| **Fit Criterion**         | Note the research source for the measurement technique of a cognitive or motor ability whenever we begin developing a game around a new ability.  |
| **Priority**              | Very high     |

| **ID**: COM-4          | **Type**: Non-functional         |
|-----------------------|--------------------|
| **Related PUC**: TODO        | **Originator**: Dr. Carette   |
| **Description**          | Maintain a meeting minutes log for each meeting we have with our supervisor.  |
| **Rationale**            | We should be meeting with our supervisor, Sasha, every two weeks. To show that we have done this and have discussed meaningful and important topics at such meetings, we should keep track of the main points discussed at a each meeting.   |
| **Fit Criterion**         | Take down notes at each meeting with the supervisor and submit a link to these notes to Avenue.  |
| **Priority**              | Medium    |

| **ID**: COM-5          | **Type**: Non-functional         |
|-----------------------|--------------------|
| **Related PUC**: TODO        | **Originator**: Jack   |
| **Description**          | Maintain an issue board on GitLab that indicates tasks that must be completed, who is assigned to and working on said task, when such task is due, and any comments or concerns that may arise during development on that task.  |
| **Rationale**            | In order to ensure the timely delivery of project milestones, we should be keeping an audit trail of what is left to be done and the progress done towards completing this. Doing this, we will have proof that work on the project has been done *throughout* the academic year and not rushed at the final minute. It also provides our supervisor with an idea about the state of the project and any potential milestone delays and the reasons behind such delays if any arise.     |
| **Fit Criterion**         | Keep the GitLab issue tracker up to date with respect to work done and work to be done.  |
| **Priority**              | High    |


Specify the requirements derived from existing standards or regulations, including:  
* Report format
* Data naming
* Accounting procedures
* Audit tracing

For example, this could specify the requirement for software to trace processing activity. Such traces are needed for some applications to meet minimum regulatory or financial standards. An audit trace requirement may, for example, state that all changes to a payroll database shall be recorded in a trace file with before and after values.

### 3.5 Design and Implementation

#### 3.5.1 Installation
Mini games will be provided on web browsers so no installation on software itself is required to run the games.

But to make sure the games could run stably on your web browser, a standard mini-games running environment has the following requirements:

1. A web server is required to serve the requested pages to the client browser
2. Adobe, Flash should be enabled to load the games.


(Constraints to ensure that the software-to-be will run smoothly on the target implementation platform.)

#### 3.5.2 Distribution
Constraints on software components to fit the geographically distributed structure of the host organization, the distribution of data to be processed, or the distribution of devices to be controlled.

#### 3.5.3 Maintainability
The initial mini-game set will be released to only available on web browsers, where keyboard is the main interaction tool. The initial Mini-Game set will be released by early February, 2021.

In the next step, games will be upgraded to enable player interaction through more accessory tools: mouses, touch pads, and joy-cons. To do this, an upgrade on game interfaces, back-end frameworks will be done in March, 2021 and Mini-Game Battery 2.0 will be released by the Capstone material deadline, which is April 4th, 2021.

After the graded presentation, there will be no further updates to the game.


(Specify attributes of software that relate to the ease of maintenance of the software itself. These may include requirements for certain modularity, interfaces, or complexity limitation. Requirements should not be placed here just because they are thought to be good design practices.)

#### 3.5.4 Reusability
<!-- TODO: come up with a description -->

The mini games will be designed in a modular way where the framework on interfaces, backend and ability measurement methods will be reused across the whole game design process.

At a later stage, two groups who do the same Mini-Game Battery Project will collaborate on the backend. In this way, collected data will also be shared and reused in the ability testing process.


#### 3.5.5 Portability
The project is platform and interface independent. The web browser games can be run on any PC of all modes in different operating systems.

(Specify attributes of software that relate to the ease of porting the software to other host machines and/or operating systems.)

#### 3.5.6 Cost
There will be no charge on the mini games. As the games are designed to help Sasha’s research, we would like more players to get involved and provide rich testing data. 
In the game design process, we will use open source software so there are no firm costs associated with the project. However, if the group decides to allocate money to purchase assets then those will fall into this domain. The paid assets could be: url for the web games, accessories such as mouses, joycons.
Labour would be a non-monetary cost. The project is scheduled to take 8 months to complete by five 4th year CS undergraduate students. The expected working time is approximately 8 hours per week.


(Specify monetary cost of the software product.)

#### 3.5.7 Deadline
| Date | Delivery |
| --- | ----------- |
| October 2, 2020 | Project Summary |
| October 19, 2020 | Requirement Specification |
| October 28, 2020 | Prototype 1.0 |
| November 13, 2020 | Design Specification |
| TBA | Prototype 2.0 |
| TBA | Internal and External Testing |
| **February 28, 2021** | **Initial Mini-Game Release** |
| March 31, 2021 | Mini-Game Upgrade |
| **April 5, 2021** | **Mini-Game 2.0 Release** |

Note: Upgraded Mini-Game is not part of the supervisor's basic requirements. Our group reserves the right to adjust the accomplishment of this upgrade considering our time.

(Specify schedule for delivery of the software product.)

#### 3.5.8 Proof of Concept
<!-- TODO: come up with a description -->

## 4. Verification
> This section provides the verification approaches and methods planned to qualify the software. The information items for verification are recommended to be given in a parallel manner with the requirement items in Section 3. The purpose of the verification process is to provide objective evidence that a system or system element fulfills its specified requirements and characteristics.

<!-- TODO: give more guidance, similar to section 3 -->
<!-- ieee 15288:2015 -->

## 5. Appendixes



## Cultural Requirements

| ID: NF-C1 | Type: Non-Functional |
| ---- | -------|
| PUC: N/A | Originator: Meijing |
| Description | The game must not purposefully offend/discriminate against any cultural or ethnic group. |
| Rationale |Insulting any religious or ethnic group causes conflicts and limits the potential user base. |
| Fit Criterion | Controversial content is not used nor displayed to the players. |
| Priority |High |


| ID: CU-2 | Type: Non-Functional (Cultural) |
| ---- | -------|
| PUC: N/A | Originator: Meijing |
| Description | The game shall follow industry-standard game conventions, including relevant cultural standards for a game being developed and used in Canada. |
| Rationale | Games that follow industry-standard game conventions tend to achieve efficiency and success.|
| Fit Criterion | The game will follow industry-standard game conventions and cultural standards for a game being developed and used in Canada.|
| Priority | Medium |


## Legal Requirements:
### Compliance Requirements:

| ID : CO-1 | Type : Non-functional Requirements (Compliance) |
| ---- | -------|
| Related PUC:N/A | Originator: Meijing |
| Description | The game will adhere to all terms and conditions set out by Unity. |
| Rationale | Unity’s terms and conditions need to be followed from a legal standpoint. |
| Fit Criterion | Unity license terms and agreements must be followed.|
| Priority | Very High |



| ID : CO-2 | Type : Non-functional Requirements (Compliance) |
| ---- | -------|
| Related PUC: N/A | Originator: Meijing |
| Description | The game will comply with all terms and conditions of McMaster’s Integrity policies and this course as a whole. |
| Rationale | McMaster’s terms and conditions in all these matters need to be followed from a legal standpoint.|
| Fit Criterion | McMaster University’s Academic Integrity regulations will be followed.|
| Priority | Very High |



|ID : CO-3 | Type : Non-functional Requirements (Compliance) |
| ---- | -------- |
|Related PUC:N/A | Originator: Meijing |
|Description | The game shall adhere to all terms and conditions of the license for borrowed assets. |
|Rationale | Public assets can have various requirements to use them in projects. |
|Fit Criterion | The product will follow all the terms such as disclosure and referencing.|
|Priority |Very High|


### Standards Requirements

|ID : STR-1 | Type : Non-functional Requirements (Compliance)|
| ---- | -------|
|Related PUC:N/A | Originator: Meijing |
| Description | The game will comply with all standards set out by the course and its professor. |
| Rationale | The university and professor expect these standards to be followed. |
| Fit Criterion | The game will follow standards set by the professor and the teaching assistant. |
| Priority | High |


## Project Schedule
|Tasks  |  Deadline  | What Do? |
| ---- | -------| ----- |
|Problem Statement | 10/02/2020 | A two-page statement of who/what/when/where of the problem that needs to be solved.|
| Requirement Specification | 10/19/2020| Software Requirements Specification that follows IEEE 29148 guidance|
| Demo | 10/25/2020 | First game should be designed and implemented|
| Prototype 1.0 | 10/28/2020 | A 3-min presentation on project overview and a 3-min demo to show a game interface and backend data framework|
| Design Specification | 11/03/2021| First version of Design Specification|
| Fill in audio and art factors | TBA | add audio and art into the games|
| Prototype 2.0 | TBA | All Mini-games should be implemented |
| Internal and external testing | TBA | Debugging | 
| Improve the games | 02/25/2020 | Take advice from players and supervisors to improve the games |
| Initial Mini-Game Release | 02/28/2021 | Release the first version of games |
| Mini-Game Upgrade | 03/31/2021 | Upgrade the game interface to allow more accessories to be used| 
| Mini-Game 2.0 Release | April 5th, 2021 | Release the upgraded games|

Risks:
* Time allocation: We may not balance time on each portion in the accomplishment of the project. One common problem is that not enough testing is applied to the project.
* Ability measurement: The measure standards we come up with may not quantify the motor and cognitive abilities very well, which could result in inaccurate testing results.
* The fitness of game and testing abilities: The collected data from the games may not directly reflect the ability aimed to test.


## User documentation and training:
### User documentation Requirements:
Documentation will be provided to outline the features, gameplay, game controls and other necessary background information.

### Training requirements:
At the beginning of the game, a quick-start tutorial will be provided to illustrate the features and the controls. Other than that, no outside training will be required. 

## Waiting room features:
Each game will cost no more than 60 seconds to play. Considering the time constraints and difficulty level, some component might not be fully developed:
* ……. (map to the previous functionality section)

## Ideas for solutions:
* Making use of free resources from Unity asset store to reduce the time consuming on interface design.
* Designing the games in a module way to reuse the component across the whole development process.






