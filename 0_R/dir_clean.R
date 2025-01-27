
fun_rm = function(path){
  t = list.files(path = path, full.names = T)
  if(length(t) == 0) {
    print(paste0(path, " is already empty"))
  } else{
    
    if(all(file.remove(t)) == T) {
      
      paste0("file ", t, " successfully removed") #if true
      
    } else {
      
      print(paste0("not all files in path ", path, " removed")) #if false
      
    } #end else file remove == T
  } #end else t == 0
} # end func


unlink("1_input", recursive = T)
fun_rm("2_mid")
fun_rm("3_output")
