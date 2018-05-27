  best<-function(state, outcome){
  data<-read.csv("outcome-of-care-measures.csv", colClasses="character")
  #state와 outcome에 해당하지 않는 것이 왔을 때 stop하고 invalid뜨도록 설정
  states<-data[,7]
  outcomes<-c("heart attack", "heart failure", "pneumonia")
  if((state %in% states) ==FALSE){
    stop(print("invalid state"))
  } else if((outcome %in% outcomes) == FALSE){
    stop(print("invalid outcome"))
  }
  #필요한 정보만을 담은 새로운 dataframe을 생성
  new_data<-as.data.frame(cbind(data[,2], #hospital
                                data[,7], #state
                                data[,11], #heart attack
                                data[,17], #heart failure
                                data[,23])) #pneumonia
  colnames(new_data)<-c("hospital", "state", "heart attack", "heart failure", "pneumonia")
  #state가 설정됐을 때 
  st<-new_data[which(state==new_data[,"state"]),]
  #outcome이 설정됐을 때 정보 고르고
  pick<-as.numeric(st[,outcome])
  #최소값의 row를 골라냄
  min_num<-which(min(pick, na.rm=TRUE)==pick)
  result<-st[,"hospital"][min_num]
  return(result)
}
