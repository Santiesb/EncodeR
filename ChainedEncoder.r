chainedDfTargetEncoder = function(dirInput, dirOutput, filePattern, nameTarget, digit) 
{
  for (j in 1:length(filePattern)) 
  {
    filesList = list.files(dirInput, pattern = filePattern[j])
    for (k in 1:length(filesList))
    {
      message(paste("Begining to encode the dataset", filesList[k]))
      dataTemp = readRDS(filesList[k])
      if ((nameTarget %in% names(dataTemp)) == TRUE)
      {
        dataTemp[,nameTarget] = encoder(dataTemp[,nameTarget], digit)
        nameChange = gsub(x=filesList[k], pattern = "*.rds", replacement = "_encoded.rds")
        fileNameOut = paste0(dirOutput, "/", nameChange)
        saveRDS(dataTemp, fileNameOut)
        message(paste(filesList[k], "done :D"))
      } 
      else
      {
        errorMessage = message(paste("The variable in", filesList[j], "is not called", nameTarget, ".", "Please, change and harmonise names.")) 
        stop(errorMessage)
      }
      rm(dataTemp)
    } 
  }
}

encoder = function(columnToEncrypt, digits)
{
  len = length(columnToEncrypt)
  code = vector("character", len)
  lengthLetters = length(letters)
  
  for (i in 1:len)
  {
    code[i] = paste(round(runif(min = 0, max = 7, n = digits)), collapse = "")
    
    if (columnToEncrypt[i] == 0)
	  {
      substr(code[i], 55,55) <- sample(letters[1:(lengthLetters/2)] 1, replace= TRUE)
	  } else if (columnToEncrypt[i] == 1)
	  {
      substr(code[i], 55,55) <- sample(letters[(lengthLetters/2+1):lengthLetters], 1, replace= TRUE)
	  } else 
	  {
	  substr(code[i], 55,55) <- sample(paste(letters, c(1:9)), 1, replace= TRUE)
	  }
  }
  return(code)
}