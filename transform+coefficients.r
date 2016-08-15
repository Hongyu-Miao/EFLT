require("igraph")
source("circle-finding-funs.r")
source("coefficients-funs.r")

#g<-read_graph("DCG-example2.gml",format="gml")
#g<-read_graph("DCG-example.gml",format="gml")
g<-read_graph("DCG-6-nodes.gml",format="gml")
hasLoopsFlag<-vcount(g)!=count_components(g,mode="strong")
notes_string_list<-list()

while(hasLoopsFlag)
{
    #the max SCC are chosen to be transformed in each loop
    comp_Res<-components(g, mode="strong")
    max_Cluster_Ind<-match(max(comp_Res[["csize"]]),comp_Res[["csize"]])
    max_Cluster_Nodes<-which(comp_Res[["membership"]] %in% max_Cluster_Ind)
    all_circles<-FindCircles(g)
    denominator_string<-GetDenominator(all_circles)
    each_in_nodes<-ego(g,1,max_Cluster_Nodes,mode="in",mindist=1)
    each_Input_Nodes<-list()
    each_Input_Num<-vector()
    all_Outside_Input_Nodes<-vector()
    for(i in 1:length(each_in_nodes))
    {
         tmp_Vec<-each_in_nodes[[i]][! each_in_nodes[[i]] %in% max_Cluster_Nodes]
         each_Input_Nodes<-c(each_Input_Nodes,list(tmp_Vec))
         each_Input_Num<-c(each_Input_Num,length(tmp_Vec))
         all_Outside_Input_Nodes<-c(all_Outside_Input_Nodes,tmp_Vec)
    }
    all_Outside_Input_Nodes<-unique(all_Outside_Input_Nodes)              #all input nodes of the current loop analysis subgraph 
    max_Input_Node_Index<-which(each_Input_Num %in% max(each_Input_Num))  #choose the nodes with the max outside inputs
    max_Input_Nodes<-max_Cluster_Nodes[max_Input_Node_Index]
    feedback_Input_Node<-max_Input_Nodes[1]
    if(length(max_Input_Nodes)>1)
    {
        in_Degree<-degree(g,max_Input_Nodes,mode="in")
        max_In_Degree_Index<-which(in_Degree %in% max(in_Degree))   #choose the nodes with the max in-degree
        max_In_Degree_Nodes<-max_Input_Nodes[max_In_Degree_Index]
        feedback_Input_Node<-max_In_Degree_Nodes[1]                 
        if(length(max_In_Degree_Nodes)>1)
        {
            out_Degree<-degree(g,max_Input_Nodes,mode="out")
            max_out_Degree_Index<-which(out_Degree %in% max(out_Degree))   #choose the nodes with the max out-degree
            max_Output_Nodes<-max_Input_Nodes[max_out_Degree_Index]
            feedback_Input_Node<-max_Output_Nodes[1]        #choose any one node with the same outside inputs, in-degree and out-degree
        }
    }
    #modify the coefficient of direct input edges
    tmp_Input_Nodes<-ego(g,1,feedback_Input_Node,mode="in",mindist=1)
    direct_Input_Nodes<-tmp_Input_Nodes[[1]][! tmp_Input_Nodes[[1]] %in% max_Cluster_Nodes]
    if(length(direct_Input_Nodes)>0)
    {
        for(i in 1:length(direct_Input_Nodes))
        {
            numerator_string<-GetNumerator(g,direct_Input_Nodes[i],feedback_Input_Node,all_circles)
            new_edge_label<-"c'"
            new_edge_label<-str_c(new_edge_label,toString(feedback_Input_Node),sep = "", collapse = NULL)
            new_edge_label<-str_c(new_edge_label,toString(direct_Input_Nodes[i]),sep = "", collapse = NULL)
            edge_id<-get.edge.ids(g,c(as.vector(direct_Input_Nodes[i]),feedback_Input_Node),directed=TRUE)
            E(g)[edge_id]$label<-new_edge_label
            one_coefficient_notes<-new_edge_label
            one_coefficient_notes<-str_c(one_coefficient_notes,"=(",sep = "", collapse = NULL)
            one_coefficient_notes<-str_c(one_coefficient_notes,numerator_string,sep = "", collapse = NULL)
            one_coefficient_notes<-str_c(one_coefficient_notes,")/(",sep = "", collapse = NULL)
            one_coefficient_notes<-str_c(one_coefficient_notes,denominator_string,sep = "", collapse = NULL)
            one_coefficient_notes<-str_c(one_coefficient_notes,")",sep = "", collapse = NULL)
            notes_string_list<-c(notes_string_list,list(one_coefficient_notes)) 
            
        }
    }
    
    #add new edges and coefficients
    inDirect_Input_Nodes<-all_Outside_Input_Nodes[! all_Outside_Input_Nodes %in% direct_Input_Nodes]
    if(length(inDirect_Input_Nodes)>0)
    {
         new_label_list<-list() 
         for(i in 1:length(inDirect_Input_Nodes))
         {
            numerator_string<-GetNumerator(g,inDirect_Input_Nodes[i],feedback_Input_Node,all_circles)
            new_edge_label<-"c'"
            new_edge_label<-str_c(new_edge_label,toString(feedback_Input_Node),sep = "", collapse = NULL)
            new_edge_label<-str_c(new_edge_label,toString(inDirect_Input_Nodes[i]),sep = "", collapse = NULL)
            new_label_list<-c(new_label_list,list(new_edge_label))
            
            one_coefficient_notes<-new_edge_label
            one_coefficient_notes<-str_c(one_coefficient_notes,"=(",sep = "", collapse = NULL)
            one_coefficient_notes<-str_c(one_coefficient_notes,numerator_string,sep = "", collapse = NULL)
            one_coefficient_notes<-str_c(one_coefficient_notes,")/(",sep = "", collapse = NULL)
            one_coefficient_notes<-str_c(one_coefficient_notes,denominator_string,sep = "", collapse = NULL)
            one_coefficient_notes<-str_c(one_coefficient_notes,")",sep = "", collapse = NULL)
            notes_string_list<-c(notes_string_list,list(one_coefficient_notes))
         }
         edge_Seq<-vector()
         for(i in 1:length(inDirect_Input_Nodes))
         {
              edge_Seq<-c(edge_Seq,c(inDirect_Input_Nodes[i],feedback_Input_Node))
         }
         g<-add_edges(g,edge_Seq)
         for(i in 1:length(inDirect_Input_Nodes))
         {
            edge_id<-get.edge.ids(g,c(as.vector(inDirect_Input_Nodes[i]),feedback_Input_Node),directed=TRUE)
            E(g)[edge_id]$label<-new_label_list[[i]]
         }
    }
    
    #remove all the feedback input edges of the feedback input nodes
    in_Nodes<-ego(g,1,feedback_Input_Node,mode="in",mindist=1)
    loop_In_Nodes<-in_Nodes[[1]][in_Nodes[[1]] %in% max_Cluster_Nodes]
    del_Edge_Pair<-vector()
    for(i in 1:length(loop_In_Nodes))
    {
        del_Edge_Pair<-c(del_Edge_Pair,c(as.numeric(loop_In_Nodes[i]),feedback_Input_Node)) 
    } 
    g<-delete_edges(g,get.edge.ids(g,del_Edge_Pair,directed=TRUE))
    
    hasLoopsFlag<-vcount(g)!=count_components(g,mode="strong") 
}
  
plot(g,main="Equivalent Transformation Results")
box(lty = '1373', col = 'red')
for(i in 1:length(notes_string_list))
{
    mtext(notes_string_list[[i]],side=1,line=i-1)
}




