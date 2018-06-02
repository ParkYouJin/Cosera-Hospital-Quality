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
  
  rankhospital<-function(state, outcome, num="best"){
  #data읽음
  data<-read.csv("outcome-of-care-measures.csv", colClasses="character")
  #필요한 정보만을 담은 새로운 dataframe을 생성
  new_data<-as.data.frame(cbind(data[,2], #hospital
                                data[,7], #state
                                data[,11], #heart attack
                                data[,17], #heart failure
                                data[,23])) #pneumonia
  colnames(new_data)<-c("hospital", "state", "heart attack", "heart failure", "pneumonia")
  #invalid state, outcome설정
  states<-new_data[,"state"]
  outcomes<-c("heart attack", "heart failure", "pneumonia")
  if((state %in% states)==FALSE){
    stop(print("invalid state"))
  } else if((outcome %in% outcomes)==FALSE){
    stop(print("invalid outcome"))
  } else if(is.numeric(num)){
    #해당state뽑아내기
  st<-new_data[which(state==new_data[,"state"]),]
    #numeric후 오름차순으로 order
  st[,outcome]<-as.numeric(st[,outcome])
  st<-st[order(st[,outcome], na.last=TRUE),]
  #num가 state개수 범위 내에 해당하지 않을 때 stop
  if(num >length(st[,2])){
    stop(NA)
    #state개수와 같을 때 num번째의 hostital 표시
  }else if(num<=length(st[,2])){
    result<-st[,"hospital"][num]
    return(result)
  }
  #num가 숫자가 아닐 때
  } else if(!is.numeric(num)){
    #num가 best일때 best함수 값 표시
    if(num=="best"){
      result<-best(state, outcome)
    } else if(num=="worst"){
      #rank가 worst일때 내림차순의 첫번째 hospital표시
      wt<-new_data[which(state==new_data[,"state"]),]
      wt[,outcome]<-as.numeric(wt[,outcome])
      wt<-wt[order(wt[,outcome], decreasing=TRUE, na.last=TRUE),]
      result<-wt[,"hospital"][1]
      return(result)
    }
  }
  }
  
