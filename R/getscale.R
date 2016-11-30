getscale <-
function(df,file, column, sheet=1){
    require("readxl")  
    reference <<- read_excel(file, sheet) #EXCEL
    Data<-df   #DADES
    
    f<-function(ref=reference, df=Data){ 
      Data1<-Data
      nam<-names(Data1)
      allitems1<- ref$VariablesInput #quan fem by selecciona nomes items per aquella escala
      c<- nam %in% allitems1  #busca aquests items al df
      return(c)}
    
    output<<-by(reference, reference[,column], f)
    return(output)
  }
