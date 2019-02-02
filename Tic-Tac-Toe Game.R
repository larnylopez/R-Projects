# R Tic Tac Toe by Larny Lopez

# Enter play() in the console to play Tic-Tac-Toe

#Displays the state of the board
display <- function(state){
	
	cat("\n", state[1] , "|", state[2], "|" , state[3], "\n",
	    "--+---+--", 
	    "\n", state[4], "|", state[5], "|", state[6], "\n",
	    "--+---+--" , 
	    "\n", state[7], "|" , state[8], "|", state[9], "\n")

}

update <- function(state, who, pos){
	
	if(!any(state[pos] == "x" | state[pos] == "o") ){
	#Checks if there is already an input of "x" or "o"
	
    if( who == "x" ){
		#Input x at the position
		state[pos] <- "x"
		
	  } else if(who == "o" ){
		
		  #Input o at the position
		  state[pos] <- "o"
		
	  } 
	  
	}else{
	  
		print("You cannot play here!")
	  
	}
  
	return(state)
  
}
  
  
#Computer AI when the game is set to 1 Player
computer_turn <- function(state){

	#checks if there are more x's than o's
	if(sum(state == "x") > sum(state =="o") ){
	  
	#this means than the human player went first
		comp_sign <- "o"
		human_sign <- "x"
		
	}else{
	  
	#this means computer starts first
		comp_sign <- "x"
		human_sign <- "o"
		
		}
	
  #winning combinations
	triples <- list(
  	c(1,2,3),
  	c(4,5,6),
  	c(7,8,9),
  	c(1,4,7),
  	c(2,5,8),
  	c(3,6,9),
  	c(1,5,9),
  	c(3,5,7)
  	)
	
	#number of winning combinations
	num_wins <- length(triples)
	
	for(i in 1:num_wins){
		
		#If there are two of the comp_sign
		#Attempt to make a three in a row
		if(sum(state[ triples[[i]] ] == comp_sign) == 2 ){
			
			win_index <- which(state[ triples[[i]] ] != comp_sign)
			#If the win_index is not a human sign
			#Can make a three in a row
			
			if(state[triples[[i]][win_index]] != human_sign){ 
				
				state[triples[[i]][win_index]] <- comp_sign
				display(state)
				return(state)
				
				}else{ 
				  
				  #If there is a human sign on the winning possible index, just skip iteration
					next
				  
					}
		}	
	} 
	  
	 for(i in 1:num_wins){
	   
	   #If there are two of the human_sign
	   #Prevent the human player to win
	   if(sum(state[ triples[[i]] ] == human_sign) == 2 ){

		    	lose_index <- which(!(state[ triples[[i]] ] == human_sign))

			        if(state[triples[[i]][lose_index]] != comp_sign){
			        #If the lose_index is not a computer sign already
			        #Block the human player from winning
			          
				      state[triples[[i]][lose_index]] <- comp_sign
				      display(state)
				      return(state)
				      
				      }
	    }
	 }
	
	#If there are no winning or losing situations, the computer plays randomly
	random_index <- sample(1:9,1)
		  # Check if there is an icon on the index, resample if there is
			while(state[random_index] == comp_sign | state[random_index] == human_sign){
				random_index <- sample(1:9,1)
			}
			
			state[random_index] <- comp_sign
			display(state)
			return(state)
}

#Check the board if there is a winner
check_winner <- function(state){
	
	triples <- list(
  	c(1,2,3),
  	c(4,5,6),
  	c(7,8,9),
  	c(1,4,7),
  	c(2,5,8),
  	c(3,6,9),
  	c(1,5,9),
  	c(3,5,7)
  	)

#Get the length of all the possible winning combinations
num_wins <- length(triples)


for(i in 1:num_wins){
  
	  #Loops for all possible winner combinations
	  if(all(state[ triples[[i]] ] == rep("x", 3)) ){
	    
	  	#Checks if thre is a three in a row of "x"
		print("Winner: Player x")
		return(winner <- TRUE)
		
	} else if(all(state[ triples[[i]] ] == rep("o", 3)) ){
	  
	  #Checks if there is a three in a row of "o"
		print("Winner: Player o")
		return(winner <- TRUE)
		
	} 
	
	      #Initialize empty logical vector to determine if it's a draw
	      #If all are TRUE
	      draw <- logical(length(state))
		     
	      for(i in seq_along(state)){
	        
	        #Check if the vector has x's and o's
            draw[i] <- ( all(state[i] == "x" | state[i] == "o") )
            
	      }
	      
	          #If draw is all TRUE
            if(all(draw)){
            	
              print("It's a draw!")
              return(winner <- TRUE)
             
            }
           	      
	      
	      
} 

	return(winner <- FALSE)

}

#Menu to check the number of users and to determine the order of play
users <-  function(){
	
	cat("Play Tic-Tac-Toe")
	computer <- FALSE
	go_first <- FALSE
	
	players <- readline("Choose 1 for One player or 2 for Two players:")
	
	if(players == "1"){
		
		cat("\n", "Number of players: 1")
		computer <- TRUE
		
	} else if(players == "2"){
		
		cat("\n", "Number of players: 2")
		
	}
	
	order <- readline("Choose 1 to be Player 1 or 2 to be Player 2:")
	if(order == "1"){
		
		cat("You will be: x", "\n", "Player 2 will be: o")
		go_first <- TRUE
		
	} else if(order == "2"){
	  
		cat("\n", "You will be: o", "\n", "Player 2 will be: x")
	  
		}	
		
		return(c(computer, go_first))
	
}


#Initiates the Tic-Tac-Toe Game
play <- function(){
	
	#User returns a logical vector
	#First element if for the number of players
	#Second element if for which player starts first
	settings <- users()
	
	#Checks the input from the user function
	single_player <- settings[1]
	use_x <- settings[2]
	
	#Initiatialize game
	state <- 1:9
	
	#Turns TRUE when there is a winner
	winner <- FALSE
	turns <- 1 #Number of turns to prompt the check_winner function
	
	display(state)
	
	if(single_player){
		
		while(winner == FALSE){
		
		#The human player's sign
		if(use_x){
			
			print("Turn: x")
			who <-"x"
			
			#Get an input for the position
			pos <- as.integer(readline("Choose a number:"))
			#If the position is already taken it will ask again
			while(state[pos] == "x" | state[pos] == "o"){
				cat("Invalid input!")
				pos <- as.integer(readline("Choose a number:"))
				
			}
		
		#Update the state
		state <- update(state, who, pos)
		
		display(state)
		
		#Check winner will be initialized
			if(turns >= 2){	
			  
			winner <- check_winner(state)
			
			if(winner == TRUE) break
			
			}
		
		cat("\n","Computer's move:")
		state <- computer_turn(state)
		
		#Check winner will be initialized
			if(turns >= 3){	
			  
			winner <- check_winner(state)
			
			if(winner == TRUE)	break
			
			}
		}
		  
		if(!use_x){
			
			cat("\n","Computer's move:")
			state <- computer_turn(state)
			
			#Check winner will be initialized
			if(turns >= 2){	
			winner <- check_winner(state)
			if(winner == TRUE) break
			
			}
			
			print("Turn: o")
			who <- "o"
			
			#Get an input for the position
			pos <- as.integer(readline("Choose a number:"))
			#If the position is already taken it will ask again
			while(state[pos] == "x" | state[pos] == "o"){
			  
				cat("Invalid input!")
				pos <- as.integer(readline("Choose a number:"))
				
			}
		
		#Update the state
		state <- update(state, who, pos)
		
		display(state)
		
		#Check winner will be initialized
			if(turns >= 3){	
			  
			winner <- check_winner(state)	
			
			if(winner == TRUE) break
			
			}
		
		}
		  
		turns <- turns +1
		
	}

		
	}else{
	  
		#If it is two players
		
	#Going to keep playing while there is no winner
	while(winner == FALSE){
		
		#Changes the who values for each turn
		if(use_x){
		  
			use_x <- FALSE
			print("Turn: x")
			who <-"x"
			
		}else{
		
			use_x <- TRUE
			print("Turn: o")
			who <- "o"
		}
		
		#Get an input for the position
		pos <- as.integer(readline("Choose a number:"))
		
		#If the position is already taken it will ask again
		while(state[pos] == "x" | state[pos] == "o"){
		  
			cat("Invalid input!")
			pos <- as.integer(readline("Choose a number:"))
			
		}
		
		#Update the state
		state <- update(state, who, pos)
		
		display(state)
		
		#Check winner will be initialized, turn starts at 1 (turn == 6 is 5 turns)
		if(turns >= 6){
			
			winner <- check_winner(state)
				
		}
		
		#keeps track of the rurns
		turns <- turns + 1
		
	}
	}
}