;; Author: Nate Beckemeyer

;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;; Set up templates
;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
(deftemplate objct
  (slot name) (slot location) (slot edible?) (slot isa) (slot used))
(deftemplate person
  (slot location) (slot Credits) (slot Salary) (slot Moves) (slot Ate) (slot Slept))
(deftemplate place
  (slot name) (slot north) (slot south) (slot east) (slot west) (slot info))
(deftemplate door
  (slot name) (slot from) (slot to) (slot status) (slot direction))
(deftemplate mode (slot status))
;(deftemplate input (slot command) (slot argument))

;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;; Set up templates
;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;; YOU ARE ENCOURAGED TO ADD/ALTER PLACES AND THEIR "info" DESCRIPTIONS
(deffacts TU
  (mode (status start))
  (place (name Graduation) (south ACAC)
	 (info "Your cap falls off now and then, your gown does'nt fit you but your frozen smile shows it all!"))
  (place (name Summer_Job) (east ACAC) (info "Is'nt this supposed to be a summer camp?"))
  (place (name 4253) (east Keplinger) (info "Most fun I have ever had in a course!!"))
  (place (name 1043) (east McFarlin) (info "You got to start somewhere."))
  (place (name Dorm_Room) (east McClure) (info "All work and no sleep ... "))
  (place (name Cafetaria) (east Skelly) (info "Lots to eat; mind your cholesterol"))
  (place (name Orientation) (north Skelly) (info "Welcome to wonderland!"))
  (place (name Skelly) (north McClure) (south Orientation) (east Kendall)
	 (west Cafetaria) (info "Go Blue"))
  (place (name McClure) (north McFarlin) (south Skelly) (east Registration) (west Dorm_Room)
	 (info "Empty your pockets here.  Right now"))
  (place (name McFarlin) (north Keplinger) (south McClure) (east 2003) (west 1043)
	 (info "A quiet place to ..."))
  (place (name Keplinger) (north ACAC) (south McFarlin) (east 3123) (west 4253)
	 (info "Home, sweet home"))
  (place (name ACAC) (north Graduation) (south Keplinger) (east Job_Interview)
	 (west Summer_Job) (info "Food and fun, for everyone"))
  (place (name Job_Interview) (west ACAC) (info "Gimme that, gimme that"))
  (place (name 3123) (west Keplinger) (info "Another day, another course"))
  (place (name 2003) (west McFarlin) (info "Now you are coding"))
  (place (name Registration) (west McClure) (info "Enlist and serve"))
  (place (name Kendall) (west Skelly) (info "If you can act, sing, dance, ... the stage awaits"))
  (objct (name pizza) (location Cafetaria) (isa food) (used 0))
  (objct (name salad) (location Cafetaria) (isa food) (used 0))
  (objct (name map) (location Orientation) (isa paper))
  (objct (name schedule) (location Registration) (isa paper))
  (objct (name diploma) (location Graduation) (isa paper))
  (objct (name food) (edible? yes) (isa Object))
  (objct (name paper) (edible? no) (isa Object))
  (objct (name Object))
  (door (name sum_job) (from ACAC) (direction west) (status closed))
  (door (name job_int) (from ACAC) (direction east) (status closed))
  (door (name grad) (from ACAC) (direction north) (status closed))
  (person (location Skelly) (Credits 0) (Salary 0) (Moves 0) (Ate 0) (Slept 0)))

;; IN THE FOLLOWING I HAVE PROVIDED THE SKELETONS FOR SOME, BUT NOT ALL, OF THE
;; PRODUCTIONS THAT YOU NEED TO WRITE FOR THIS GAME
;; NOTE: THESE SKELETONS REPRESENT HOW I IMPLEMENTED THIS GAME.  YOU ARE NOT
;; REQUIRED TO FOLLOW THIS SKELETON.

;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;;  Production that detects a starting position and performs necessary makes to set up
;; the environment.
;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
(defrule start
   ?mode <- (mode (status start))
   =>
   (modify ?mode (status run))
   (printout t crlf "***************************")
   (printout t crlf "*                         *")
   (printout t crlf "* College:  The Adventure *")
   (printout t crlf "*         By              *")
   (printout t crlf "*      Sandip Sen         *")
   (printout t crlf "***************************" crlf))

;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;;  Prompts the player for next input and reads it in
;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
(defrule Read
   ?mode <- (mode (status run))
   =>
   (modify ?mode (status run))
   (printout t crlf crlf "*** What next?  ")
   (assert (input (explode$ (readline)))))

;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;;  Response to an invalid command
;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
(defrule Dont-Understand
   ?input <- (input $?x)
   ?player <- (person (Moves ?m))
   =>
   (retract ?input)
   (modify ?player (Moves (+ 1 ?m)))
   (printout t crlf "Invalid command"))

;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;; Allows you to stop and restart very easy -- just type run again.
;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
(defrule quit
 ?input <- (input  stop|quit|exit|bye)
 ?player <- (person (Moves ?z))
  =>
 (retract ?input)
 (modify ?player (Moves (+ 1 ?z)))
 (halt))

;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;; Providing help to the player
;; YOU ARE ENCOURAGED TO ADD OTHER SYNONYMS FOR COMMANDS AND HANDLE THEM APPROPRIATELY
;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
(defrule help
  ?input <- (input help)
  ?player <- (person (Moves ?z))
  =>
  (retract ?input)
  (modify ?player (Moves (+ 1 ?z)))
  (printout t crlf "Quit: Stops the game")
  (printout t crlf "Describe: Describes the current area and objects in the location")
  (printout t crlf "Directions: N, S, E, and W to move in those directions.")
  (printout t crlf "Get: Gets an object in the area. If on object is specified, gets the most recently dropped items.")
  (printout t crlf "Drop: Drops an object. If no object is specified, drops the most recently obtained item.")
  (printout t crlf "Eat: Eats an edible object. If no object is specified, eats the most recently obtained edible item.")
  (printout t crlf "Sleep: Sleeps. A player can only sleep inside a dorm room.")
  (printout t crlf "Status: Prints out status information." crlf)
)


;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;;  Describing the current location of the player when he possesses the campus map.
;; NOTE: superfluous conditions and attributes have been added so that combined with
;; recency and specificity this rule fires before the immediately following ones.
;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
(defrule location_info
  (mode (status run))
   ?input <- (input describe|look)
   ?person <- (person (location ?x) (Moves ?z))
   (objct (name map) (location player))
   (place (name ?x) (info ?y&~nil))
   =>
   (printout t crlf "You are at " ?x crlf ?y crlf crlf))

;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;;   The following productions prompt user about the places to the north, south, east
;; and west (if present) to the current location if he has the campus map
;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
(defrule see_north
  (mode (status run))
  ?input <- (input describe|look)
  (objct (name map) (location player))
  (person (location ?x))
  (place (name ?x) (north ?y&~nil))
   =>
   (printout t "To the north is " ?y crlf)
)

(defrule see_south
  (mode (status run))
  (input describe|look)
  (objct (name map) (location player))
  (person (location ?x))
  (place (name ?x) (south ?y&~nil))
   =>
   (printout t "To the south is " ?y crlf)
)

(defrule see_east
  (mode (status run))
  (input describe|look)
  (objct (name map) (location player))
  (person (location ?x))
  (place (name ?x) (east ?y&~nil))
   =>
   (printout t "To the east is " ?y crlf)
)

(defrule see_west
  (mode (status run))
  (input describe|look)
  (objct (name map) (location player))
  (person (location ?x))
  (place (name ?x) (west ?y&~nil))
   =>
   (printout t "To the west is " ?y crlf)
)


;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;;  Due to necessarily matching multiple conditions while in the describe|look input,
;;  I chose to make a special rule for retracting the input, slightly less specific than
;;  the previous rules.
;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
(defrule look_retraction
 ?input <- (input describe|look)
 (objct (name map) (location player))
 ?person <- (person (location ?x) (Moves ?m))
 (place (name ?x))
 =>
   (retract ?input)
   (modify ?person (Moves (+ 1 ?m)))
 )

;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;;  Describing current location to the user if he does not have campus map. NOTE: there
;;  is a superfluous 4th condition on LHS to make this rule fire before see_objcts.
;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
(defrule without_map_info
  (mode (status run))
  ?input <- (input look|describe)
  ?player <- (person (Moves ?m))
  (not (objct (name map) (location player)))
   =>
   (retract ?input)
   (modify ?player (Moves (+ 1 ?m)))
  (printout t crlf "If you had a map, you could see neighboring areas!" crlf)
)


;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;;  Describes whatever the player can see in the current location
;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
(defrule see_objcts
  (mode (status run))
  ?player <- (person (location ?x))
  (objct (name ?y) (location ?x))
   =>
   (printout t crlf "You see object " ?y "." crlf)
)

;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;;  Lists the objcts in possession of the player
;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
(defrule have_objcts
  (mode (status run))
  (person (Moves ?m))
  (input status|score|inventory)
  (objct (name ?x) (location player))
  =>
  (printout t crlf "You have item " ?x " in your inventory.")
)

;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;; Prints status information
;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
(defrule current_status
   ?input <- (input status|score|inventory)
   ?player <- (person (Moves ?m))
   (person (Credits ?c) (Salary ?s) (Ate ?a) (Slept ?sl) (Moves ?m))
   =>
   (retract ?input)
   (modify ?player (Moves (+ 1 ?m)))
   (printout t crlf "You have " ?c  " credits.")
   (printout t crlf "Your salary is " ?s)
   (printout t crlf "You have eaten " ?a " times.")
   (printout t crlf "You have slept " ?sl " times.")
   (printout t crlf "You have issued " (+ 1 ?m) " commands (including this one)."))

;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;;	The following productions processes the directional movement command by first
;;  changing the position as asked and then describing the new position with the help
;;  of other productions which describes current location of the player. Also increments
;;  the number of moves made by the player.
;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
(defrule go_south
  ?input <- (input s|south)
  ?person <- (person (location ?x) (Moves ?z)) ;|)
  (place (name ?x) (south ?y&~nil))
   =>
   (retract ?input)
   (modify ?person (location ?y) (Moves (+ 1 ?z)))
)

(defrule go_north
  ?input <- (input n|north)
  ?person <- (person (location ?x) (Moves ?z)) ;|)
  (place (name ?x) (north ?y&~nil))
   =>
   (retract ?input)
   (modify ?person (location ?y) (Moves (+ 1 ?z)))
)

(defrule go_east
  ?input <- (input e|east)
  ?person <- (person (location ?x) (Moves ?z)) ;|)
  (place (name ?x) (east ?y&~nil))
   =>
   (retract ?input)
   (modify ?person (location ?y) (Moves (+ 1 ?z)))
)

(defrule go_west
  ?input <- (input w|west)
  ?person <- (person (location ?x) (Moves ?z)) ;|)
  (place (name ?x) (west ?y&~nil))
   =>
   (retract ?input)
   (modify ?person (location ?y) (Moves (+ 1 ?z)))
)

;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;;	The following production checks if a closed door prevents the player from
;;  moving in the direction he desires, and prompts him so. NOTE: only 3 doors are
;;  closed and the player can be stopped by these if he tries to move either east, west
;;  or north from ACAC and the corresponding door is closed.
;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
(defrule closed_door_north
  ?input <- (input n|north)
  (door (direction north) (status closed))
   =>
   (retract ?input)
   (printout t crlf "A closed door is blocking your path, plebeian!" crlf)
)
(defrule closed_door_west
  ?input <- (input w|west)
  (door (direction west) (status closed))
  (person (location ACAC))
  =>
  (retract ?input)
  (printout t crlf "A closed door is blocking your path! Why don't you try going to class?" crlf)
)
(defrule closed_door_east
  ?input <- (input e|east)
  (door (direction east) (status closed))
  (person (location ACAC))
  =>
  (retract ?input)
  (printout t crlf "A closed door is blocking your path! Get a job, nerd!" crlf)
)

;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;;	Less specific default rule to handle the case where it is not possible to
;;  carry out players request for directional movement (i.e., a wall blocks his way).
;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
(defrule wall
  ?input <- (input n|north|e|east|w|west|s|south) ;|)
  =>
  (printout t crlf "OUCH! That's a WALL!!.")
  (retract ?input))

;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;;	Allows the player to sleep only if he is in his Dorm room; increments number of
;;  moves and number of times he slept.
;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
(defrule to_sleep
  (mode (status run))
  ?input <- (input sleep|rest)
  ?person <- (person (location Dorm_Room) (Slept ?x) (Moves ?z))
  =>
  (retract ?input)
  (printout t crlf "Ahhh... Nothing like four hours of sleep to get you through the week." crlf)
  (modify ?person (Slept (+ 1 ?x)) (Moves (+ 1 ?z)))
)

;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;;	Prevents player from sleeping at any other position.
;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
(defrule cant_sleep
  (mode (status run))
  (input sleep|rest)
  (not (person (location Dorm_Room)))
  =>
  (printout t crlf "You can't sleep there! There is no suitable bed for your majesty." crlf)
)

;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;;	If the player asks for a certain objct, and if the objct happens to be located
;;  at the same position as the player, then the location field of the objct is changed
;;  to `player', i.e., the player is given the objct. Increments number of moves.
;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
(defrule get_specific_objct
  ?input <- (input get|take|pick|pickup ?z)
  ?player <- (person (location ?y) (Moves ?x))
  ?item <- (objct (name ?z) (location ?y))
  =>
  (retract ?input)
  (modify ?player (Moves (+ 1 ?x)))
  (modify ?item (location player))
  (printout t crlf "Target acquired. Excellent work!" crlf))
)

;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;;	The objct asked for by the player is not at the same location.
;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
(defrule cannot_get_specific_objct
  ?input <- (input get|take|pick|pickup ?z)
  (person (location ?y))
  (not (objct (name ?z) (location player)))
  =>
  (retract ?input)
  (printout t crlf "Error 404: Resource " ?z " could not be located." crlf)
)

;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;;	Handles the command without arguments, by finding any objct present at that
;;  location.
;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
(defrule get_any_objct
  ?input <- (input get|take|pick|pickup)
  ?player <- (person (location ?x) (Moves ?z))
  ?item <- (objct (name ?y) (location ?x))
  =>
  (retract ?input)
  (modify ?item (location player))
  (modify ?player (Moves (+ 1 ?z)))
  (printout t crlf "You obtained resource " ?y ". Congratulations!" crlf)
)

;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;;	There is no objct to pick up in response to a get command without arguments
;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
(defrule cannot_get_any_objct
 (mode (status run))
 ?input <- (input get|take|pick|pickup)
 (person (location ?x))
 (not (objct (location ?x)))
  =>
  (retract ?input)
  (printout t crlf "No objects in your current location!" crlf)
)

;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;;	The following productions regenerate campus map, and Dorm foods
;;  when one is taken by the player.
;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
(defrule regenerate_map
 (mode (status run))
 (person (location Orientation))
 ?item <- (objct (name map) (location player))
 =>
 (duplicate ?item (location Orientation))
)

(defrule regenerateFood
 (mode (status run))
 ?item <- (objct (name ?z) (location player) (isa food) (used ?m))
 (not (objct (name ?z) (location Cafetaria)))
  =>
  (duplicate ?item (location Cafetaria) (used (+ 1 ?m)))
)

;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;;	The player is allowed to eat an objct if he possesses that objct and the
;; objct belongs to a class (isa) of objcts that is edible.
;; Increments number of moves and number of times he ate.
;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
(defrule eat_specific_objct
  ?input <- (input eat ?z)
  ?item <- (objct (name ?z) (location player) (isa food))
  ?player <- (person (Ate ?y) (Moves ?x))
  =>
  (modify ?player (Ate (+ 1 ?y)) (Moves (+ 1 ?x)))
  (retract ?item)
  (retract ?input)
  (printout t crlf "You ate item " ?z ". Scrumptious!" crlf)
)

;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;;	The player is prevented from eating an objct which belongs to a class of
;;  objcts that is not edible.
;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
(defrule not_edible_objct
  (input eat ?z)
  (not (objct (name ?z) (location player) (isa food)))
  =>
  (printout t crlf "You cannot eat " ?z "! What an attempt." crlf)
)

;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;;	Handles the command eat without any argument. Checks to see if the player has
;;  anything in his possession that belongs to a class of objcts which are edible.
;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
(defrule eat_anything
 ?input <- (input eat)
 ?item <- (objct (name ?z) (location player) (isa food))
 ?player <- (person (Ate ?y) (Moves ?m))
  =>
  (modify ?player (Ate (+ 1 ?y)) (Moves (+ 1 ?m)))
  (retract ?item)
  (retract ?input)
  (printout t crlf "You ate object " ?z "! Yummy yummy in my tummy." crlf)
)

;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;;    The player has nothing that he can eat.
;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
(defrule cannot_eat_anything
 (input eat)
 (not (objct (location player) (isa food)))
  =>
  (printout t crlf "You have nothing to eat, silly! Try going shopping at the caf." crlf)
)

;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;;	The player allowed to drop a specific objct if he has that in his possession.
;;  Increments the number of moves.
;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
(defrule drop_specific_objct
 ?input <- (input drop|give|submit ?z)
 ?item <- (objct (name ?z) (location player))
 ?player <- (person (location ?x) (Moves ?m))
  =>
  (retract ?input)
  (modify ?item (location ?x))
  (modify ?player (Moves (+ 1 ?m)))
  (printout t crlf "You dropped object " ?z "! Better not need it!" crlf)
)

;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;;	The player cannot drop anything that he does not possess.
;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
(defrule cannot_drop_specific_objct
 (input drop|give|submit ?z)
 (not (objct (name ?z) (location player)))
  =>
  (printout t crlf "Cannot drop item " ?z " because you don't have it, silly!" crlf)
)

;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;;	In response to a drop command without arguments, finds the first thing in
;;  possession of the player and fires the more specific rule with this objct to drop.
;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
(defrule drop_any_objct
  ?input <- (input drop|give|submit)
  ?item <- (objct (name ?y) (location player))
  ?player <- (person (location ?x) (Moves ?m))
  =>
  (retract ?input)
  (modify ?player (Moves (+ 1 ?m)))
  (modify ?item (location ?x))
  (printout t crlf "Dropped item " ?y "! You should probably pick it back up." crlf)
)

;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;;	There is nothing in possession of the player to drop.
;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
(defrule cannot_drop_any_objct
 (input drop|give|submit)
 (not (objct (location player)))
  =>
  (printout t crlf "You have no items to drop, dummy!" crlf)
)

;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;;  Production to aid player if he loses campus map
;;  Arbitrarily included (mode (status run)) to ensure that the player knows
;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
(defrule lost
  (mode (status run))
  (not (objct (name map) (location player)))
  (not (person (location Orientation)))
   =>
   (printout t crlf "You should head south to orientation to get a map if you need it!" crlf)
)

;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;;  The following rules assign credits to the player. If the first course taken by
;;  the player is 1043 he gets one credit, other courses taken first fetches him .5
;;  credits. Any subsequent course taken fetches an additional 1 credit.
;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
(defrule give_first_credit_1
 (objct (name schedule) (location 2003|3123|4253) (isa paper))
 ?player <- (person (location 2003|3123|4253) (Credits 0))
 =>
 (modify ?player (Credits -.5))
)

;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;;	The following productions create the different grade objcts when the proper
;;  grade (for prerequisite course) or the class_schedule is dropped in a classroom.
;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
(defrule create_1043_grade
  ?schedule <- (objct (name schedule) (location 1043))
  ?player <- (person (location 1043) (Credits ?y))
  =>
  (modify ?player (Credits (+ ?y 1)))
  (modify ?schedule (name 1043Grade))
  (printout t crlf "You have passed class 1043! Be sure to get your grade." crlf)
)

(defrule create_2003_grade
 (or ?paperthing <- (objct (name schedule) (location 2003))
     ?paperthing <- (objct (name 1043Grade) (location 2003)))
 ?person <- (person (location 2003) (Credits ?y))
  =>
  (modify ?paperthing (name 2003Grade))
  (printout t crlf "You have (barely) passed class 2003! Be sure to get your grade." crlf)
  (modify ?person (Credits (+ 1 ?y)))
)
(defrule create_3123_grade
 (or ?paperthing <- (objct (name schedule) (location 3123))
     ?paperthing <- (objct (name 2003Grade) (location 3123)))
 ?person <- (person (location 3123) (Credits ?y))
  =>
  (modify ?paperthing (name 3123Grade))
  (printout t crlf "You rocked 3123! Congratulations! Be sure to get your grade." crlf)
  (modify ?person (Credits (+ 1 ?y)))
)
(defrule create_4253_grade
 (or ?paperthing <- (objct (name schedule) (location 4253))
     ?paperthing <- (objct (name 3123Grade) (location 4253)))
 ?person <- (person (location 4253) (Credits ?y))
  =>
  (modify ?paperthing (name 4253Grade))
  (printout t crlf "Never have you experienced more pain and punishment than in 4253... Pick up your grade." crlf)
  (modify ?person (Credits (+ 1 ?y)))
)

;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;;	The door to summer job is opened when the player gets the grade for any course
;;  and the door is not already open.
;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
(defrule open_summer_job
 (objct (name 1043Grade|2003Grade|3123Grade|4253Grade) (location player))
 ?door <- (door (name sum_job) (status closed))
  =>
  (modify ?door (status open))
  (printout t crlf "You can now get a summer job! Woooo! Just don't be a lifeguard..." crlf)
)

(defrule open_job_interview
 (person (location Summer_Job))
 ?door <- (door (name job_int) (status closed))
 =>
 (modify ?door (status open))
 (printout t crlf "You are now ready to take on a real job!" crlf)
)

(defrule open_graduation
 (objct (name 4253Grade) (location player))
 ?door <- (door (name grad) (status closed))
  =>
  (modify ?door (status open))
  (printout t crlf "You can now graduate!" crlf)
)

;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;;	Graduation door closed if the player does not have 4253 grade with him
;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
(defrule close_graduation
  ?door <- (door (direction north) (status open))
  (objct (name 4253Grade) (location ?x&~player))
  =>
  (printout t crlf "Closing graduation door" crlf)
  (modify ?door (status closed)))

;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;;	Salary fixed according to the number of credits earned once player moves into
;;  the permanent job interview room.
;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
(defrule assign_salary
 ?input <- (input e|east)
 ?player <- (person (Credits ?x) (location ACAC))
 (door (direction east) (status open))
  =>
  (retract ?input)
  (modify ?player (Salary (* 10000 ?x)) (location Job_Interview))
  (printout t crlf "Look at all that sick dosh! You're now making $" (* 10000 ?x) "!" crlf)
)

;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;;	Once the player gets diploma, the game is over. He is congratulated, and
;;  his credits, salary, number of moves and computed score is printed out.
;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
(defrule the_end
  ?player <- (person (Credits ?c) (Slept ?x) (Ate ?y) (Salary ?s) (Moves ?m))
  (objct (name diploma) (location player))
  =>
  (printout t crlf "Congratulations on completing your college adventure at TU!")
  (if (not (and (>= 4 (+ ?x ?y)) (> ?x 0) (> ?y 0))) then
   (modify ?player (Credits (- ?c 1)))
   (bind ?c (- ?c 1)))
  (printout t crlf "You have " ?c " credits.")
  (printout t crlf "Your salary will be $" ?s)
  (printout t crlf "You took " ?m " moves.")
  (printout t crlf "Your final score is : " (/ (* ?c ?s) ?m) crlf)
  (halt))

;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;;  Retraction
;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
(defrule retract_all
  ?player <- (person (Moves ?z))
  (or ?input <- (input drop|give|submit|eat|get|take|pick|pickup|sleep|rest|status|score|inventory|describe|look)
      ?input <- (input drop|give|submit|eat|get|take|pick|pickup|sleep|rest|status|score|inventory|describe|look ?z))
  =>
  (retract ?input)
  (modify ?player (Moves (+ 1 ?z))))