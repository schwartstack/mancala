# 1:6 player board (0:4)
# 7 player home (0:48)
# 8:13 computer's board (0:4)
# 14 computer's home (0:48)
# 15 speed (.1:1)
# 16 sound (0,1)
# 17 difficulty (1,2)
# 18 start player (1,2)
# 19 current move (1:6,8:13)

if(!("beepr" %in% rownames(installed.packages()))){
  install.packages("beepr")
}
if(!("audio" %in% rownames(installed.packages()))){
  install.packages("audio")
}
library(beepr)
library(audio)

launch.mancala<-function(){
  n=" __    __     ______     __   __     ______     ______     __         ______    
/\\ \"-./  \\   /\\  __ \\   /\\ \"-.\\ \\   /\\  ___\\   /\\  __ \\   /\\ \\       /\\  __ \\   
\\ \\ \\-./\\ \\  \\ \\  __ \\  \\ \\ \\-.  \\  \\ \\ \\____  \\ \\  __ \\  \\ \\ \\____  \\ \\  __ \\  
 \\ \\_\\ \\ \\_\\  \\ \\_\\ \\_\\  \\ \\_\\\\\"\\_\\  \\ \\_____\\  \\ \\_\\ \\_\\  \\ \\_____\\  \\ \\_\\ \\_\\ 
  \\/_/  \\/_/   \\/_/\\/_/   \\/_/ \\/_/   \\/_____/   \\/_/\\/_/   \\/_____/   \\/_/\\/_/ 
  "
  
  
  n=strsplit(n,"")[[1]]
  
  for(i in 1:length(n)){
    cat(n[i])
    Sys.sleep(.01)
    if(i<length(n)/2){
      if(i%%10==0){
        chime(i%%3+1)
      }
    }else{
      if(i%%5==0){
        chime(i%%3+1)
      }
    }
  }
  beep(11)
  Sys.sleep(1)
  beep(11)
  cat("(c) 2019 | Jonathan Schwartz | jzs1986@gmail.com","\n")
  Sys.sleep(1)
  while(T){
    chime(1)
    difficulty<-readline(prompt="Select difficulty (easy, medium): ")
    if(difficulty!="easy"&&difficulty!="medium"){
      cat("Invalid entry.","\n")
    }else if(difficulty=="easy"){
      difficulty=1
      break
    }else{
      difficulty=2
      break
    }
  }
  while(T){
    chime(1)
    speed<-readline(prompt="Select speed (1-10): ")
    if(as.numeric(speed)%in%1:10){
      speed=1/as.numeric(speed)
      break
    }else{
      cat("Invalid entry.","\n")
    }
  }
  while(T){
    chime(1)
    sound<-readline(prompt="Sound on? (yes, no): ")
    if(sound!="0"&&sound!="1"&&sound!="on"&&sound!="off"&&sound!="yes"&&sound!="no"){
      cat("Invalid entry.","\n")
    }else{
      if(sound=="0"||sound=="no"||sound=="off"){
        sound=0
        break
      }else{
        sound=1
        break
      }
    }
  }
  chime(1)
  first<-readline(prompt="Would you like to go first? (yes, no): ")
  if(startsWith(first,"y")){
    first=1
  }else if(startsWith(first,"n")){
    first=0
  }else{
    first=sample(0:1,1)
  }
  chime(1)
  x=mancala(x=c(rep(4,6),0,rep(4,6),0,speed,sound,difficulty,first,NA))
  while(T){
    if(x[17]==1){
      lb=read.table("mancala.lb.easy.txt",header = T)
      if(x[7]>23){
        scores=sort(as.numeric(c(lb[,"Score"],x[7])),decreasing = T)
        place=max(which(scores==x[7]))
        if(place<11){
          cat("Congradulations! You have high score #",place,"\n",sep="")
          Sys.sleep(.5)
          name=readline(prompt="Enter your name: ")
          df=data.frame("Name"=name,"Score"=x[7],"Date"=format(Sys.Date(),"%m/%d/%y"))
          lb=rbind(lb,df)
          lb=lb[order(lb[,"Score"],decreasing = T),]
          lb=lb[-11,]
          row.names(lb)=1:nrow(lb)
          write.table(lb,"mancala.lb.easy.txt")
          cat("Easy Leaderboard","\n")
          print(lb)
          Sys.sleep(.5)
        }
      }
    }else{
      lb=read.table("mancala.lb.medium.txt",header = T)
      if(x[7]>23){
        scores=sort(as.numeric(c(lb[,"Score"],x[7])),decreasing = T)
        place=max(which(scores==x[7]))
        if(place<11){
          cat("Congradulations! You have high score #",place,"\n",sep="")
          Sys.sleep(.5)
          name=readline(prompt="Enter your name: ")
          df=data.frame("Name"=name,"Score"=x[7],"Date"=format(Sys.Date(),"%m/%d/%y"))
          lb=rbind(lb,df)
          lb=lb[order(lb[,"Score"],decreasing = T),]
          lb=lb[-11,]
          row.names(lb)=1:nrow(lb)
          write.table(lb,"mancala.lb.medium.txt")
          cat("Medium Leaderboard","\n")
          print(lb)
          Sys.sleep(.5)
        }
      }
    }
    again<-readline(prompt="Would you like to play again? ")
    if(startsWith(again,"y")){
      if(x[16]==1){
        chime(1)
      }
      first<-readline(prompt="Would you like to go first? (yes, no): ")
      if(startsWith(first,"y")){
        first=1
      }else if(startsWith(first,"n")){
        first=0
      }else{
        first=sample(0:1,1)
      }
      x=mancala(c(rep(4,6),0,rep(4,6),0,x[15:17],first,NA))
    }else{
      cat("Goodbye.","\n")
      break
    }
  }
}

mancala<-function(x=c(rep(4,6),0,rep(4,6),0,.8,1,2,1,NA)){
  if(x[18]==1){
    while(T){
      if(sum(x[1:6])>0&&sum(x[8:13])>0){
        x=player.turn(x)
      }else{
        break
      }
      if(sum(x[1:6])>0&&sum(x[8:13])>0){
        if(x[17]==1){
          x=ai.turn.easy(x)
        }else if(x[17]==2){
          x=ai.turn.medium(x)
        }
      }else{
        break
      }
    }
  }else{
    while(T){
      print.board(x)
      Sys.sleep(1)
      if(sum(x[1:6])>0&&sum(x[8:13])>0){
        if(x[17]==1){
          x=ai.turn.easy(x)
        }else if(x[17]==2){
          x=ai.turn.medium(x)
        }
      }else{
        break
      }
      if(sum(x[1:6])>0&&sum(x[8:13])>0){
        x=player.turn(x)
      }else{
        break
      }
    }
  }
  if(sum(x[1:6])==0){
    for(i in 8:13){
      if(x[i]>0){
        x[14]=x[14]+x[i]
        x[i]=0
        if(x[16]==1){
          beep(2)
        }
        print.board(x)
      }
    }
  }else{
    for(i in 1:6){
      if(x[i]>0){
        x[7]=x[7]+x[i]
        x[i]=0
        if(x[16]==1){
          beep(2)
        }
        print.board(x)
      }
    }
  }
  if(x[7]>x[14]){
    cat("You win!","\n")
    if(x[16]==1){
      beep(3)
    }
  }else if(x[7]<x[14]){
    cat("The computer has won.","\n")
    if(x[16]==1){
      beep(7)
    }
  }else{
    cat("It's a tie.","\n")
    if(x[16]==1){
      beep(10)
    }
  }
  return(x)
}

player.turn<-function(x){
  turn=T
  while(turn){
    print.board(x)
    x=get.move(x)
    if(!is.na(x[19])){
      if(!(x[x[19]]+x[19]==7)&&!(x[x[19]]+x[19]==20)){
        turn=F
      }
    }
    x=do.move(x)
    if(sum(x[1:6])==0||sum(x[8:13])==0){
      break
    }
  }
  return(x)
}

ai.turn.easy<-function(x){
  turn=T
  while(turn){
    while(T){
      x[19]=sample(8:13,1)
      if(x[x[19]]!=0){
        break
      }
    }
    cat("Computer selects house",x[19]-7,"\n")
    Sys.sleep(1.5)
    if(!(x[x[19]]+x[19]==14)&&!(x[x[19]]+x[19]==27)){
      turn=F
    }
    x=do.move(x)
    if(sum(x[1:6])==0||sum(x[8:13])==0){
      turn=F
    }
  }
  return(x)
}

ai.turn.medium<-function(x){
  turn=T
  while(turn){
    goodmove=F
    okmoves=NULL
    aggromoves=NULL
    
    computerboard=x[8:13]
    max=max(computerboard)
    maxhouses=which(computerboard==max)
    if(length(maxhouses)==1){
      x[19]=maxhouses+7
    }else{
      x[19]=sample(maxhouses,1)+7
    } #move is a house with most stones (dumb move)
    
    for(i in 1:6){
      if(x[i]==0&&x[14-i]>0){
        okmoves[length(okmoves)+1]=14-i
        # cat("found an ok move","\n",okmoves,"\n")
        # Sys.sleep(2)
      }
    } #if defensive move exists, added to pool of ok moves
    
    for(i in 13:9){
      if(x[i]==0&&x[14-i]>0){
        for(j in (i-1):8){
          if(x[j]==i-j){
            aggromoves[length(aggromoves)+1]=j
            # cat("found an aggro move","\n",aggromoves,"\n")
            # Sys.sleep(2)
          }
        }
      }
    } #if aggromove exists, added to pool of aggro moves
    
    for(i in 13:8){
      if((x[i]==14-i)||(x[i]==27-i)){
        x[19]=i
        goodmove=T
        # cat("found a good move","\n",move,"\n")
        # Sys.sleep(2)
        break #if pefect finish in cup 9:13 exists that is new move.
      }
    }
    
    if(!goodmove&&is.null(aggromoves)&&!is.null(okmoves)){
      if(length(okmoves)==1){
        x[19]=okmoves
      }else{
        x[19]=sample(okmoves,1)
      }
    }
    
    if(!goodmove&&!is.null(aggromoves)){
      if(length(aggromoves)==1){
        x[19]=aggromoves
      }else{
        x[19]=sample(aggromoves,1)
      }
    }
    
    if(goodmove&&!is.null(aggromoves)){
      x[19]=max(c(aggromoves,x[19]))
    }
    
    cat("Computer selects house",x[19]-7,"\n")
    Sys.sleep(1.5)
    if(!(x[x[19]]+x[19]==14)&&!(x[x[19]]+x[19]==27)){
      turn=F
    }
    x=do.move(x)
    if(sum(x[1:6])==0||sum(x[8:13])==0){
      turn=F
    }
  }
  return(x)
}

do.move<-function(x){
  stones=x[x[19]]
  x[x[19]]=0
  extra=0
  if(x[16]==1){
    chime(1)
  }
  print.board(x)
  if(x[19]<7){
    for(i in 1:stones){
      if((x[19]+i)%%14==0){
        extra=extra+1
      }else{
        x[(x[19]+i)%%14]=x[(x[19]+i)%%14]+1
        if(x[16]==1){
          
            chime(2)
        }
        print.board(x)
      }
    }
    if(extra==0){
      if(x[19]+stones<7&&x[x[19]+stones]==1&&x[14-x[19]-stones]>0){
        x[7]=x[7]+x[14-x[19]-stones]+1
        x[14-x[19]-stones]=0
        x[x[19]+stones]=0
        if(x[16]==1){
          beep(2)
        }
        print.board(x)
      }
    }else if(extra>0){
      for(j in 1:extra){
        x[(x[19]+i+j)%%14]=x[(x[19]+i+j)%%14]+1
        if(x[16]==1){
          chime(2)
        }
        print.board(x)
      }
      if((x[19]+stones+extra)%%14<7&&x[(x[19]+stones+extra)%%14]==1&&x[14-(x[19]+stones+extra)%%14]>0){
        x[7]=x[7]+x[14-(x[19]+stones+extra)%%14]+1
        x[14-(x[19]+stones+extra)%%14]=0
        x[(x[19]+stones+extra)%%14]=0
        if(x[16]==1){
          beep(2)
        }
        print.board(x)
      }
    }
  }else if(x[19]>7){
    for(i in 1:stones){
      if((x[19]+i)%%14==0){
        x[14]=x[14]+1
        if(x[16]==1){
         
            chime(2)
          
        }
        print.board(x)
      }else if((x[19]+i)%%14==7){
        extra=extra+1
      }else{
        x[(x[19]+i)%%14]=x[(x[19]+i)%%14]+1
        if(x[16]==1){
          chime(2)
        }
        print.board(x)
      }
    }
    if(extra==0){
      if(x[19]+stones<14&&x[x[19]+stones]==1&&x[14-x[19]-stones]>0){
        x[14]=x[14]+x[14-x[19]-stones]+1
        x[14-x[19]-stones]=0
        x[x[19]+stones]=0
        if(x[16]==1){
          beep(2)
        }
        print.board(x)
      }
    }else if(extra>0){
      for(j in 1:extra){
        x[(x[19]+i+j)%%14]=x[(x[19]+i+j)%%14]+1
        if(x[16]==1){
          chime(2)
        }
        print.board(x)
      }
      if((x[19]+stones+extra)%%14>7&&(x[19]+stones+extra)%%14<14&&x[(x[19]+stones+extra)%%14]==1&&x[14-(x[19]+stones+extra)%%14]>0){
        x[14]=x[14]+x[14-(x[19]+stones+extra)%%14]+1
        x[14-(x[19]+stones+extra)%%14]=0
        x[(x[19]+stones+extra)%%14]=0
        if(x[16]==1){
          beep(2)
        }
        print.board(x)
      }
    }
  }
  return(x)
}

get.move<-function(x){
  while(T){
    if(x[16]==1){
      chime(4)
    }
    x[19]<-readline(prompt="select your move: ")
    if(startsWith(x[19],"option")||startsWith(x[19],"help")){
      cat("Enter \"leaderboard\" to view the high scores.","\n")
      if(x[16]==1){
        chime(1)
      }
      Sys.sleep(.5)
      cat("Enter \"clear leaderboard\" to erase the high scores.","\n")
      if(x[16]==1){
        chime(1)
      }
      Sys.sleep(.5)
      cat("Enter \"sound on\" or \"sound off\" to turn sound on or off.","\n")
      if(x[16]==1){
        chime(1)
      }
      Sys.sleep(.5)
      cat("Enter \"speed\" followed by a digit 1-10 to change the game speed.","\n")
      if(x[16]==1){
        chime(1)
      }
      Sys.sleep(.5)
      cat("Enter a number 1-6 to select a move.","\n")
      if(x[16]==1){
        chime(1)
      }
      Sys.sleep(.5)
      if(x[16]==1){
        chime(4)
      }
      readline(prompt = "Press [Enter] to return to game.")
      print.board(x)
    }
    if(startsWith(x[19],"leader")||startsWith(x[19],"high")){
      cat("Easy Leaderboard","\n")
      if(x[16]==1){
        chime(1)
      }
      Sys.sleep(.5)
      lb1=read.table("mancala.lb.easy.txt",header=T)
      lb2=read.table("mancala.lb.medium.txt",header=T)
      if(nrow(lb1)>0){
        print(lb1)
        cat("\n")
      }else{
        cat("No high scores yet.","\n")
        cat("\n")
      }
      if(x[16]==1){
        chime(1)
      }
      Sys.sleep(.5)
      cat("Medium Leaderboard","\n")
      if(x[16]==1){
        chime(1)
      }
      Sys.sleep(.5)
      if(nrow(lb2)>0){
        print(lb2)
      }else{
        cat("No high scores yet.")
      }
      if(x[16]==1){
        chime(1)
      }
      Sys.sleep(.5)
      if(x[16]==1){
        chime(4)
      }
      readline(prompt = "Press [Enter] to return to game.")
      print.board(x)
    }
    if(startsWith(x[19],"erase")||startsWith(x[19],"clear")){
      if(x[16]==1){
        chime(4)
      }
      sure=readline(prompt="Are you sure you want to clear the high scores? ")
      if(startsWith(sure,"y")){
        df=data.frame(Name=0,Score=0,Date=0)
        write.table(df[-1,],"mancala.lb.easy.txt")
        write.table(df[-1,],"mancala.lb.medium.txt")
        Sys.sleep(.5)
        if(x[16]==1){
          chime(1)
        }
        cat("High scores have been erased.","\n")
        Sys.sleep(.5)
        if(x[16]==1){
          chime(4)
        }
        readline(prompt = "Press [Enter] to return to game.")
        print.board(x)
      }else{
        print.board(x)
      }
    }
    if(x[19]=="sound on"||x[19]=="sound off"){
      cat("Sound has been turned",strsplit(x[19]," ")[[1]][2],"\n")
      if(strsplit(x[19]," ")[[1]][2]=="on"){
        x[16]=1
        Sys.sleep(1)
      }else{
        x[16]=0
        Sys.sleep(1)
      }
    }else if(startsWith(x[19],"speed")){
      if(as.numeric(strsplit(x[19]," ")[[1]][2])%in%1:10){
        x[15]=1/as.numeric(strsplit(x[19]," ")[[1]][2])
        cat("Speed has been changed to",strsplit(x[19]," ")[[1]][2],"\n")
        Sys.sleep(1)
      }else{
        cat("Invalid move.","\n")
      }
    }else if(!(as.numeric(x[19]%in%1:6))||x[as.numeric(x[19])]==0){
      if(x[19]!="help"&&x[19]!="options"&&!startsWith(x[19],"leader")&&!startsWith(x[19],"high")&&!startsWith(x[19],"clear")&&!startsWith(x[19],"erase")){
        cat("Invalid move.","\n")}
      Sys.sleep(.5)
    }else{
      return(as.numeric(x))
    }
  }
}

print.board<-function(x){
  board=c(" ________________________________________________________________ ",
          "            (6)     (5)     (4)     (3)     (2)     (1)",
          " ________________________________________________________________ ",
          "|  ____    ____    ____    ____    ____    ____    ____          |",
          "| |    |  [____]  [____]  [____]  [____]  [____]  [____]   ____  |",
          "| |    |                                                  |    | |",
          "| |____|   ____    ____    ____    ____    ____    ____   |    | |",
          "|         [____]  [____]  [____]  [____]  [____]  [____]  |____| |",
          "|________________________________________________________________|",
          "           (1)     (2)     (3)     (4)     (5)     (6)")
  if(x[7]<10){
    substr(board[7],61,61)<-as.character(x[7])
  }else{
    substr(board[7],61,62)<-as.character(x[7])
  }
  if(x[14]<10){
    substr(board[6],5,5)<-as.character(x[14])
  }else{
    substr(board[6],5,6)<-as.character(x[14])
  }
  for(i in 1:6){
    target=(i-1)*8+13
    if(x[i]<10){
      substr(board[8],target,target)<-as.character(x[i])
    }else{
      substr(board[8],target,target+1)<-as.character(x[i])
    }
  }
  for(i in 8:13){
    target=54-(i-8)*8
    if(x[i]<10){
      substr(board[5],target,target)<-as.character(x[i])
    }else{
      substr(board[5],target-1,target)<-as.character(x[i])
    }
  }
  for(i in 1:length(board)){
    cat(board[i],"\n")
  }
  Sys.sleep(x[15])
}

chime<-function(n){
  if(n==1){
    play(sin(1:10000/100),rate=150000)
  }else if(n==2){
    play(sin(1:10000/60),rate=200000)
  }else if(n==3){
    play(sin(1:10000/60),rate=150000)
  }else if(n==4){
    play(sin(1:10000/89),rate=200000)
  }
}