library(beepr)
library(audio)

level=list(c(0,0,0,0,2,0,rep(0,8),.5,1,1,NA),
           c(0,0,0,0,2,1,rep(0,8),.5,1,1,NA),
           c(0,0,0,3,1,0,rep(0,8),.5,1,1,NA),
           c(0,0,0,3,1,1,rep(0,8),.5,1,1,NA),
           c(0,0,4,2,0,0,rep(0,8),.5,1,1,NA),
           c(0,0,4,2,0,1,rep(0,8),.5,1,1,NA),
           c(0,0,4,2,2,0,rep(0,8),.5,1,1,NA),
           c(0,0,4,2,2,1,rep(0,8),.5,1,1,NA),
           c(0,5,3,1,1,0,rep(0,8),.5,1,1,NA),
           c(0,5,3,1,1,1,rep(0,8),.5,1,1,NA),
           c(6,4,2,0,0,0,rep(0,8),.5,1,1,NA),
           c(6,4,2,0,0,1,rep(0,8),.5,1,1,NA),
           c(6,4,2,0,2,0,rep(0,8),.5,1,1,NA),
           c(6,4,2,0,2,1,rep(0,8),.5,1,1,NA),
           c(6,4,2,3,1,0,rep(0,8),.5,1,1,NA),
           c(6,4,2,3,1,1,rep(0,8),.5,1,1,NA))

mancala.puzzle<-function(n=1){
  while(T){
    cat("Level",n,"\n")
    chime(1)
    Sys.sleep(1)
    beep(11)
    x=level[[n]]
    print.board(x)
    life=T
    while(life){
      x=get.move(x)
      if(x[x[19]]+x[19]!=7){
        life=F
      }
      x=do.move(x)
      if(sum(x[1:6])==0){
        break
      }
    }
    if(sum(x[1:6])==0&&sum(x[8:13])==0){
      if(n==16){
        cat("You win! Congratulations!")
        beep(3)
        Sys.sleep(2)
        break
      }else{
        beep(5)
        cat("Level complete!","\n")
        enter=readline("Press [Enter] for next level.")
        mancala.puzzle(n+1)
      }
    }else{
      beep(9)
      cat("You lose.","\n")
      Sys.sleep(.5)
      again=readline("Try again? ")
      if(startsWith(again,"y")){
        mancala.puzzle(n)
      }else{
        cat("Goodbye.","\n")
        break
      }
    }
    break
  }
}