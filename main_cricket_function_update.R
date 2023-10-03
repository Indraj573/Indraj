Cricket_Match <- function(Team_Data_1 , Team_Data_2,Team_name_1,Team_name_2 ){
  names <- c(deparse(substitute(Team_data_1)), deparse(substitute(Team_Data_2)))
  x <- strsplit(x = names , split = "_", fixed = TRUE)
  x <- sapply(x,"[[",1)
  Team_1 <- x[1]
  Team_2 <- x[2]
  
  
  
  ball <- NULL
  for(i in 1:11){
    ball[i] <- player_data$Team_5[[i]]$bowling
  }
  bowler_seq <- NULL
  for ( i in 1:5){
    bowler_seq[i] <- which(ball == max(ball), arr.ind = TRUE)[1]
    ball[bowler_seq[i]] <- 0
  }
  
  
  
  
  Baller_name <- NULL
  B_run <- NULL
  B_wicket <- NULL
  
  Order <- rep(bowler_seq,10)
 
  
  batsman_name <- NULL
  Batsman_6 <- NULL
  Batsman_4 <- NULL
  batsman_run <- NULL
  batsman_outer <- NULL
  
  
  
  Runs <- 0
  Ind <- 1
  
  for(i in 1:50){
    
    
    if(Ind == 11){
      break
    }
    Over <- NULL
    
    
    Or <- Order[i]
    
    Wicket <- 0
    
    
    for(j in 1:6){
      if(Ind == 11){
        break
      }
      
      
      Sam <- sample(c("Out","Not_Out"),1,replace = TRUE,prob = c(Team_Data_2[Or]$bowling,1-Team_Data_2[Or]$bowling))
      
      if(Sam == "Not_Out"){
        Run <- sample(c(0,1,2,3,4,6),1,replace = TRUE,prob = Team_Data_1[Ind]$Batting)
        Runs <- Runs + Run
        over[j] <- Run
        batsman_name[6(i-1) + j] <- Team_name_1[ind]
        batsman_run[6(i-1) + j] <- Run
        
        if(Run == 1 || Run == 3){
         
          
          
          nam <- Team_name_1[ind]
          nam_ <- Team_name_1[ind + 1]
          Team_name_1[ind + 1] <- nam
          Team_name_1[ind] <- nam_
          
          
          
          Runner <- Team_Data_1[Ind]$Batting
          Facer <-Team_Data_1[Ind +1]$Batting
          Team_Data_1[Ind+1]$Batting <- Runner
          Team_Data_1[Ind]$Batting <- Facer
          
        } 
        
        
        if (Run == 4){
          Batsman_4[6(i-1)+j] <- 1
        }
        
        
        if (Run == 6){
          Batsman_6[6(i-1)+j] <- 1
        } 
        
        
        
      } else {
        
        
        batsman_outer[6(i-1)+j] <- Team_name_2[or]
        Ind <- Ind + 1
        Wicket <- Wicket + 1
        
      }
      if (j == 6 ){
        
        nam <- Team_name_1[ind]
        nam_ <- Team_name_1[ind + 1]
        Team_name_1[ind + 1] <- nam
        Team_name_1[ind] <- nam_
        
        
        B_wicket[i] <- Wicket
        Baller_name[i] <- Team_name_2[or]
        Ball_runs[i] <- sum(over)
        Runner <- Batting_1[Ind]
        Facer <- Batting_1[Ind + 1]
        Batting_1[Ind + 1] <- Runner
        Batting_1[Ind] <- Facer
        Over_Data[[i]] <- Over
      }
        
        
     }
      
      
      
      
    }
    
    
    
    
    
  }
  
}





