require("igraph")

source("stack-common.r")

## FindCircles: Get all simple circles from a  graph 
## inputg: the input graph 
## return value: a list of simple circle paths  

##this algorithm is from: Johnson, D. B. (1975). "Finding all the elementary circuits of a directed graph." SIAM Journal on Computing 4(1): 77-84.

FindCircles<-function(inputg)
{
    
CircuitProcess<-function(curNodeIDv)
{
    #curNodeIDv<-1
    #--------test----------------------
    gBlocked[curNodeIDv]<<-TRUE
    #gBlocked[curNodeIDv]<-TRUE   #block the current node
    indexList<-curNodeIDv-gStep+1   #map node id to the index of the current SCC
    tmpElement<-c(curNodeIDv,gAK[[indexList]])    #the first element is the current node id,the next elements are the out-neighbours
    push(gStack,tmpElement)  #push the current node into stack
    
    while(TRUE)
    {
       backFlag<-FALSE     #backtrack flag, set faluse firstly
       while(lengthstack(gStack)>0)     #get the top element of stack
       { 
          topElement<-pop(gStack) 
          if((length(topElement)>1))    #get the top element that need to be process 
             {break}
          else      #not found, go on popping and modify block flag and backtrack flag 
          {
             #gBlocked[topElement[1]]<-FALSE
             gBlocked[topElement[1]]<<-FALSE
             backFlag<-TRUE 
          }
       }
       #in the case of backtrack, removing the last term of the top element of stack and then judge 
       if(backFlag==TRUE)
       {
          if((length(topElement)==1)&&(lengthstack(gStack)==0))  # the first element and no out-edges that need to check, and then end loop
          {
              break
          }else
          {
              topElement<-topElement[-length(topElement)]
              push(gStack,topElement)
          }
       }else
       {       
           #get the last out-neighbor node of the top element of stack
           tmpNodeIndex<-topElement[length(topElement)]
           tmpNodeIdw<-tmpNodeIndex+gStep-1 #map to node id 
           if(tmpNodeIdw==gStep)    #find one loop, it is the first term of each element of stack 
           {
                tmpElementList<-print.stack(gStack)
                tmpcircle<-numeric()
                for(i in 1:length(tmpElementList))
                {
                    tmpcircle<-c(tmpcircle,tmpElementList[[i]][1])
                }
                tmpcircle<-c(tmpcircle,topElement[1])   #add current top element of stack 
                tmpcircle<-c(tmpcircle,gStep)  #add the source node id
                circlelist<<-append(circlelist,list(tmpcircle))
                #circlelist<-append(circlelist,list(tmpcircle)) 
                
                #delete the last term of current element and push it into stack 
                topElement<-topElement[-length(topElement)]
                push(gStack,topElement) 
           }
           else  #not find a loop 
           { 
                if(gBlocked[tmpNodeIdw]==FALSE)   #the last term is not in the stack and push stack 
                {
                    push(gStack,topElement) #push the current element firstly 
                    tmpElement<-c(tmpNodeIdw,gAK[[tmpNodeIndex]])  
                    push(gStack,tmpElement)
                    #gBlocked[tmpNodeIdw]<-TRUE   #after push, change block flag 
                    gBlocked[tmpNodeIdw]<<-TRUE
                }else  #the last term is in stack and then delete the last term of current element, then push stack 
                {
                    topElement<-topElement[-length(topElement)]
                    push(gStack,topElement) 
                }
           }
    }   #end of backFlag else 
  }   #end of while

}


    circlelist<-list()
    #deal with self-loop firstly
    tmplogicvec<-is.loop(inputg)
    if(sum(tmplogicvec)>0)
    {
        for(i in 1:sum(tmplogicvec))
        {
            tmpedgesid<-match(TRUE,tmplogicvec)
            tmplogicvec[tmpedgesid]<-FALSE
            circlelist<-append(circlelist,list(get.edge(inputg,tmpedgesid)))
        }
    }
    inputg<-simplify(inputg)
    
    #deal with other loops 
    totalNodes<-vcount(inputg)
    gAK<-list()
    #initiate gBK
    gBK<-list()
    for(i in 1:totalNodes)
    {
        gBK<-append(gBK,list(numeric()))
    }
    
    gBlocked<-rep(FALSE,totalNodes)
    gStack<-stack()    
    
    gStep<-1
    tmpg<-inputg
    while(gStep<totalNodes)    
    {
        tmpvertexvec<-gStep:totalNodes
        tmpg<-induced.subgraph(inputg,tmpvertexvec,impl="copy_and_delete")
        subdiff<-gStep-1
        
        sccres<-clusters(tmpg, mode="strong")
        #get the corresponding cluster of current node for looking for loops 
        
       vertexvec<-numeric()
       for(j in 1:vcount(tmpg))
       {
            if(sccres[["membership"]][j]==sccres[["membership"]][1])
            {
                vertexvec<-c(vertexvec,j)
            }
       }
       sccgraph<-induced.subgraph(tmpg,vertexvec,impl="copy_and_delete")
       for(j in 1:length(vertexvec))
       {
            vertexvec[j]<-vertexvec[j]+gStep-1 
       }  
       gAK<-get.adjlist(sccgraph,mode="out")
       if(length(unlist(gAK))>1)  #a non-self-loop include at least two edges
        {
            #gStep<-min(unlist(gAK))+gStep-1+subdiff
            gStep<-min(unlist(gAK))+subdiff
            for(i in gStep:totalNodes)
            {
                gBlocked[i]<-FALSE
                gBK[[i]]<-numeric()
            }
            #CircuitProcess_C<-cmpfun(CircuitProcess)
            #CircuitProcess_C(gStep)
            CircuitProcess(gStep)
        }
      
       gStep<-gStep+1       
    }
   
    return(circlelist)
}


