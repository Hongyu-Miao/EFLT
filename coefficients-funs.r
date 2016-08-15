require("stringi")
require("stringr")
source("circle-finding-funs.r")

GetDenominator<-function(input_all_circles)
{
    #test
    #input_all_circles<-all_circles
    #--------
    #找到所有不相交的环的组合情况
    #先确定可能的最高组合项个数 ：最长环节点个数的一半
    loop_length<-vector()
    for(i in 1:length(input_all_circles))
    {
        loop_length<-c(loop_length,length(input_all_circles[[i]])-1)
    }
    max_terms<-floor(max(loop_length)/2)
    all_order_group<-list()  #保存各项的组合情况，每阶为一个list，每项也为一个list,组合的各项为一个向量，一个组合也为一个向量
    first_order_group<-list()
    for(i in 1:length(input_all_circles))
    {
        first_order_group<-c(first_order_group,list(list(list(i))))
    }
    all_order_group<-c(all_order_group,list(first_order_group))
    if(max_terms>1)
    {
        for(i in 2:max_terms) #i为组合项个数，即阶数
        {
            find_valid_Flag<-FALSE
            cur_Order<-list()
            for(j in 1:(length(input_all_circles)-i+1))
            {
                cur_Index_List<-list()
                combination_num<-length(all_order_group[[i-1]][[j]][[1]])
                
                if(combination_num>0)
                {
                    for(k in 1:combination_num)
                    {
                        #计算每一个组合项的当前已经包含的节点
                        cur_Nodes<-vector()
                        cur_Terms<-all_order_group[[i-1]][[j]][[1]][[k]]
                        cur_Terms_List<-list()
                        for(l in 1:(i-1))
                        {
                            loop_index<-all_order_group[[i-1]][[j]][[1]][[k]][l]
                            cur_Nodes<-c(cur_Nodes,input_all_circles[[loop_index]])
                        }
                        cur_Nodes<-unique(cur_Nodes)
                        start_index<-max(all_order_group[[i-1]][[j]][[1]][[k]])+1
                        if(start_index<length(input_all_circles)||start_index==length(input_all_circles))
                        {
                            for(m in start_index:length(input_all_circles))
                            {
                                common_node_index<-which(cur_Nodes %in% input_all_circles[[m]])
                                if(length(common_node_index)==0)  #does not include any common nodes，find a comibinable term
                                {
                                    new_Terms<-c(cur_Terms,m)
                                    find_valid_Flag<-TRUE
                                    cur_Terms_List<-c(cur_Terms_List,list(new_Terms))
                                }
                            }
                        }
                        cur_Index_List<-c(cur_Index_List,list(cur_Terms_List))
    
                    }
                }
               cur_Order<-c(cur_Order,list(cur_Index_List))
            }  #end for j

            if(find_valid_Flag)
            {
                all_order_group<-c(all_order_group,list(cur_Order))
            }
            else
            {
                break;
            }
        }
    }#end if max_terms
    
    #generate the expression string
    denominator_string<-"1"
    for(i in 1:length(all_order_group))
    {
        if((i%%2)==1)
        {
            symbol_string<-"-"
        }
        else
        {
            symbol_string<-"+"
        }
        for(j in 1:length(all_order_group[[i]]))
        {
            if(length(all_order_group[[i]][[j]][[1]])>0)
            {
                for(k in 1:length(all_order_group[[i]][[j]][[1]]))
                {
                    times_Term<-""
                    for(l in 1:length(all_order_group[[i]][[j]][[1]][[k]])) #one product
                    {
                        cur_Loop_Index<-all_order_group[[i]][[j]][[1]][[k]][l]
                        for(m in 1:(length(input_all_circles[[cur_Loop_Index]])-1)) #one loop
                        {
                            edge_string<-"c"
                            edge_string<-str_c(edge_string,toString(input_all_circles[[cur_Loop_Index]][m+1]),sep = "", collapse = NULL)
                            edge_string<-str_c(edge_string,toString(input_all_circles[[cur_Loop_Index]][m]),sep = "", collapse = NULL)
                            times_Term<-str_c(times_Term,edge_string,sep = "", collapse = NULL)
                        }
                    }
                    denominator_string<-str_c(denominator_string,symbol_string,sep = "", collapse = NULL)
                    denominator_string<-str_c(denominator_string,times_Term,sep = "", collapse = NULL)
                }
            }
        }
    }
    
   return(denominator_string)    
}


GetNumerator<-function(g,start_Node,end_Node,all_circles)
{
     #start_Node<-direct_Input_Nodes[i]
#     end_Node<-feedback_Input_Node
     #test---------------
     all_paths<-all_simple_paths(g,start_Node,end_Node,mode="out")
    
     numerator_string<-""
     for(ii in 1:length(all_paths))
     {
         path_string<-""
         for(j in 1:(length(all_paths[[ii]])-1))
         {
            edge_id<-get.edge.ids(g,c(as.vector(all_paths[[ii]][j]),as.vector(all_paths[[ii]][j+1])),directed=TRUE)
            edge_label<-E(g)[edge_id]$label
            path_string<-str_c(path_string,edge_label,sep = "", collapse = NULL)
         }
         
         #get availaible loops
         available_circles<-list()
         for(j in 1:length(all_circles))
         {
             common_nodes<-all_paths[[ii]][all_paths[[ii]] %in% all_circles[[j]]]
             if(length(common_nodes)==0)  #find an available circle
             {
                 available_circles<-c(available_circles,list(all_circles[[j]])) 
             } 
         }
         
        path_expression_string<-"" 
        if(length(available_circles)>0)
        { 
            loop_length<-vector()
            for(i in 1:length(available_circles))
            {
                loop_length<-c(loop_length,length(available_circles[[i]])-1)
            }
            max_terms<-floor(max(loop_length)/2)
            all_order_group<-list()  #保存各项的组合情况，每阶为一个list，每项也为一个list,组合的各项为一个向量，一个组合也为一个向量
            first_order_group<-list()
            for(i in 1:length(available_circles))
            {
                first_order_group<-c(first_order_group,list(list(list(i))))
            }
            all_order_group<-c(all_order_group,list(first_order_group))
            if(max_terms>1)
            {
                for(i in 2:max_terms) #i为组合项个数，即阶数
                {
                    find_valid_Flag<-FALSE
                    cur_Order<-list()
                    for(j in 1:(length(available_circles)-i+1))
                    {
                        cur_Index_List<-list()
                        combination_num<-length(all_order_group[[i-1]][[j]][[1]])
                        
                        if(combination_num>0)
                        {
                            for(k in 1:combination_num)
                            {
                                #计算每一个组合项的当前已经包含的节点
                                cur_Nodes<-vector()
                                cur_Terms<-all_order_group[[i-1]][[j]][[1]][[k]]
                                cur_Terms_List<-list()
                                for(l in 1:(i-1))
                                {
                                    loop_index<-all_order_group[[i-1]][[j]][[1]][[k]][l]
                                    cur_Nodes<-c(cur_Nodes,available_circles[[loop_index]])
                                }
                                cur_Nodes<-unique(cur_Nodes)
                                start_index<-max(all_order_group[[i-1]][[j]][[1]][[k]])+1
                                if(start_index<length(available_circles)||start_index==length(available_circles))
                                {
                                    for(m in start_index:length(available_circles))
                                    {
                                        common_node_index<-which(cur_Nodes %in% available_circles[[m]])
                                        if(length(common_node_index)==0)  #does not include any common nodes，find a comibinable term
                                        {
                                            new_Terms<-c(cur_Terms,m)
                                            find_valid_Flag<-TRUE
                                            cur_Terms_List<-c(cur_Terms_List,list(new_Terms))
                                        }
                                    }
                                }
                                cur_Index_List<-c(cur_Index_List,list(cur_Terms_List))
            
                            }
                        }
                       cur_Order<-c(cur_Order,list(cur_Index_List))
                    }  #end for j
        
                    if(find_valid_Flag)
                    {
                        all_order_group<-c(all_order_group,list(cur_Order))
                    }
                    else
                    {
                        break;
                    }
                }
            }#end if max_terms
            
            #generate the expression string of the current path and its corresponding LAS
            path_expression_string<-str_c(path_expression_string,path_string,sep = "", collapse = NULL)
            for(i in 1:length(all_order_group))
            {
                if((i%%2)==1)
                {
                    symbol_string<-"-"
                }
                else
                {
                    symbol_string<-"+"
                }
                for(j in 1:length(all_order_group[[i]]))
                {
                    if(length(all_order_group[[i]][[j]][[1]])>0)
                    {
                        for(k in 1:length(all_order_group[[i]][[j]][[1]]))
                        {
                            product_Term<-""
                            for(l in 1:length(all_order_group[[i]][[j]][[1]][[k]])) #one product
                            {
                                cur_Loop_Index<-all_order_group[[i]][[j]][[1]][[k]][l]
                                for(m in 1:(length(available_circles[[cur_Loop_Index]])-1)) #one loop
                                {
                                    edge_string<-"c"
                                    edge_string<-str_c(edge_string,toString(available_circles[[cur_Loop_Index]][m+1]),sep = "", collapse = NULL)
                                    edge_string<-str_c(edge_string,toString(available_circles[[cur_Loop_Index]][m]),sep = "", collapse = NULL)
                                    product_Term<-str_c(product_Term,edge_string,sep = "", collapse = NULL)
                                }
                            }
                            product_Term<-str_c(path_string,product_Term,sep = "", collapse = NULL)
                            path_expression_string<-str_c(path_expression_string,symbol_string,sep = "", collapse = NULL)
                            path_expression_string<-str_c(path_expression_string,product_Term,sep = "", collapse = NULL)
                        }
                    }
                }
            }
        }
        else
        {
            path_expression_string<-str_c(path_expression_string,path_string,sep = "", collapse = NULL)
        } 
        if(ii>1)
        {
            numerator_string<-str_c(numerator_string,"+",sep = "", collapse = NULL)         
        }
        numerator_string<-str_c(numerator_string,path_expression_string,sep = "", collapse = NULL)  
     }#end of for ii all path
     return(numerator_string) 
}

#
#s<-"test"
#stri_length(s)
#         4%%2
##g<-read_graph("DCG-6-nodes.gml",format="gml")
#g<-read_graph("DCG-example.gml",format="gml")
#circle_list<-FindCircles(g)
#path_list<-FindForwardPaths(g,3,4)
#
#plot(g)