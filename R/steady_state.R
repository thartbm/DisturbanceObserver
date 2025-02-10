
getColors <- function() {
  
  cols.op <- c(rgb(255, 147, 41,  255, max = 255), # orange:  21, 255, 148
               rgb(229, 22,  54,  255, max = 255), # red:    248, 210, 126
               rgb(207, 0,   216, 255, max = 255), # pink:   211, 255, 108
               rgb(127, 0,   216, 255, max = 255), # violet: 195, 255, 108
               rgb(0,   19,  136, 255, max = 255)) # blue:   164, 255, 68
  
  cols.tr <- c(rgb(255, 147, 41,  32,  max = 255), # orange:  21, 255, 148
               rgb(229, 22,  54,  32,  max = 255), # red:    248, 210, 126
               rgb(207, 0,   216, 32,  max = 255), # pink:   211, 255, 108
               rgb(127, 0,   216, 32,  max = 255), # violet: 195, 255, 108
               rgb(0,   19,  136, 32,  max = 255)) # blue:   164, 255, 68
  
  cols <- list()
  cols$op <- cols.op
  cols$tr <- cols.tr
  
  return(cols)
  
}


getSteadyStateData <- function() {
  
  df <- structure(list(participant = c("1198b8", "17ec56", "1f3e12", 
                                       "2b5c2d", "2e67bf", "3c17f4", "3ef016", "3fbace", "5b81c7", "6da56a", 
                                       "8917a0", "8e9c97", "91d4f1", "1.79769313486232E+308", "9991d2", 
                                       "9c0c3a", "KURE5239", "KURE5323", "KURE5650", "KURE5668", "KURE5743", 
                                       "KURE6292", "KURE7339", "KURE7660", "KURE7732", "ad58c9", "ba465e", 
                                       "c49c82", "d2c8f6", "fc6817", "1198b8", "17ec56", "1f3e12", "2b5c2d", 
                                       "2e67bf", "3c17f4", "3ef016", "3fbace", "5b81c7", "6da56a", "8917a0", 
                                       "8e9c97", "91d4f1", "1.79769313486232E+308", "9991d2", "9c0c3a", 
                                       "KURE5239", "KURE5323", "KURE5650", "KURE5668", "KURE5743", "KURE6292", 
                                       "KURE7339", "KURE7660", "KURE7732", "ad58c9", "ba465e", "c49c82", 
                                       "d2c8f6", "fc6817", "1198b8", "17ec56", "1f3e12", "2b5c2d", "2e67bf", 
                                       "3c17f4", "3ef016", "3fbace", "5b81c7", "6da56a", "8917a0", "8e9c97", 
                                       "91d4f1", "1.79769313486232E+308", "9991d2", "9c0c3a", "KURE5239", 
                                       "KURE5323", "KURE5650", "KURE5668", "KURE5743", "KURE6292", "KURE7339", 
                                       "KURE7660", "KURE7732", "ad58c9", "ba465e", "c49c82", "d2c8f6", 
                                       "fc6817", "1198b8", "17ec56", "1f3e12", "2b5c2d", "2e67bf", "3c17f4", 
                                       "3ef016", "3fbace", "5b81c7", "6da56a", "8917a0", "8e9c97", "91d4f1", 
                                       "1.79769313486232E+308", "9991d2", "9c0c3a", "KURE5239", "KURE5323", 
                                       "KURE5650", "KURE5668", "KURE5743", "KURE6292", "KURE7339", "KURE7660", 
                                       "KURE7732", "ad58c9", "ba465e", "c49c82", "d2c8f6", "fc6817", 
                                       "1198b8", "17ec56", "1f3e12", "2b5c2d", "2e67bf", "3c17f4", "3ef016", 
                                       "3fbace", "5b81c7", "6da56a", "8917a0", "8e9c97", "91d4f1", "1.79769313486232E+308", 
                                       "9991d2", "9c0c3a", "KURE5239", "KURE5323", "KURE5650", "KURE5668", 
                                       "KURE5743", "KURE6292", "KURE7339", "KURE7660", "KURE7732", "ad58c9", 
                                       "ba465e", "c49c82", "d2c8f6", "fc6817", "1198b8", "17ec56", "1f3e12", 
                                       "2b5c2d", "2e67bf", "3c17f4", "3ef016", "3fbace", "5b81c7", "6da56a", 
                                       "8917a0", "8e9c97", "91d4f1", "1.79769313486232E+308", "9991d2", 
                                       "9c0c3a", "KURE5239", "KURE5323", "KURE5650", "KURE5668", "KURE5743", 
                                       "KURE6292", "KURE7339", "KURE7660", "KURE7732", "ad58c9", "ba465e", 
                                       "c49c82", "d2c8f6", "fc6817", "1198b8", "17ec56", "1f3e12", "2b5c2d", 
                                       "2e67bf", "3c17f4", "3ef016", "3fbace", "5b81c7", "6da56a", "8917a0", 
                                       "8e9c97", "91d4f1", "1.79769313486232E+308", "9991d2", "9c0c3a", 
                                       "KURE5239", "KURE5323", "KURE5650", "KURE5668", "KURE5743", "KURE6292", 
                                       "KURE7339", "KURE7660", "KURE7732", "ad58c9", "ba465e", "c49c82", 
                                       "d2c8f6", "fc6817", "1198b8", "17ec56", "1f3e12", "2b5c2d", "2e67bf", 
                                       "3c17f4", "3ef016", "3fbace", "5b81c7", "6da56a", "8917a0", "8e9c97", 
                                       "91d4f1", "1.79769313486232E+308", "9991d2", "9c0c3a", "KURE5239", 
                                       "KURE5323", "KURE5650", "KURE5668", "KURE5743", "KURE6292", "KURE7339", 
                                       "KURE7660", "KURE7732", "ad58c9", "ba465e", "c49c82", "d2c8f6", 
                                       "fc6817", "1198b8", "17ec56", "1f3e12", "2b5c2d", "2e67bf", "3c17f4", 
                                       "3ef016", "3fbace", "5b81c7", "6da56a", "8917a0", "8e9c97", "91d4f1", 
                                       "1.79769313486232E+308", "9991d2", "9c0c3a", "KURE5239", "KURE5323", 
                                       "KURE5650", "KURE5668", "KURE5743", "KURE6292", "KURE7339", "KURE7660", 
                                       "KURE7732", "ad58c9", "ba465e", "c49c82", "d2c8f6", "fc6817", 
                                       "1198b8", "17ec56", "1f3e12", "2b5c2d", "2e67bf", "3c17f4", "3ef016", 
                                       "3fbace", "5b81c7", "6da56a", "8917a0", "8e9c97", "91d4f1", "1.79769313486232E+308", 
                                       "9991d2", "9c0c3a", "KURE5239", "KURE5323", "KURE5650", "KURE5668", 
                                       "KURE5743", "KURE6292", "KURE7339", "KURE7660", "KURE7732", "ad58c9", 
                                       "ba465e", "c49c82", "d2c8f6", "fc6817", "1198b8", "17ec56", "1f3e12", 
                                       "2b5c2d", "2e67bf", "3c17f4", "3ef016", "3fbace", "5b81c7", "6da56a", 
                                       "8917a0", "8e9c97", "91d4f1", "1.79769313486232E+308", "9991d2", 
                                       "9c0c3a", "KURE5239", "KURE5323", "KURE5650", "KURE5668", "KURE5743", 
                                       "KURE6292", "KURE7339", "KURE7660", "KURE7732", "ad58c9", "ba465e", 
                                       "c49c82", "d2c8f6", "fc6817", "1198b8", "17ec56", "1f3e12", "2b5c2d", 
                                       "2e67bf", "3c17f4", "3ef016", "3fbace", "5b81c7", "6da56a", "8917a0", 
                                       "8e9c97", "91d4f1", "1.79769313486232E+308", "9991d2", "9c0c3a", 
                                       "KURE5239", "KURE5323", "KURE5650", "KURE5668", "KURE5743", "KURE6292", 
                                       "KURE7339", "KURE7660", "KURE7732", "ad58c9", "ba465e", "c49c82", 
                                       "d2c8f6", "fc6817", "1198b8", "17ec56", "1f3e12", "2b5c2d", "2e67bf", 
                                       "3c17f4", "3ef016", "3fbace", "5b81c7", "6da56a", "8917a0", "8e9c97", 
                                       "91d4f1", "1.79769313486232E+308", "9991d2", "9c0c3a", "KURE5239", 
                                       "KURE5323", "KURE5650", "KURE5668", "KURE5743", "KURE6292", "KURE7339", 
                                       "KURE7660", "KURE7732", "ad58c9", "ba465e", "c49c82", "d2c8f6", 
                                       "fc6817", "1198b8", "17ec56", "1f3e12", "2b5c2d", "2e67bf", "3c17f4", 
                                       "3ef016", "3fbace", "5b81c7", "6da56a", "8917a0", "8e9c97", "91d4f1", 
                                       "1.79769313486232E+308", "9991d2", "9c0c3a", "KURE5239", "KURE5323", 
                                       "KURE5650", "KURE5668", "KURE5743", "KURE6292", "KURE7339", "KURE7660", 
                                       "KURE7732", "ad58c9", "ba465e", "c49c82", "d2c8f6", "fc6817", 
                                       "1198b8", "17ec56", "1f3e12", "2b5c2d", "2e67bf", "3c17f4", "3ef016", 
                                       "3fbace", "5b81c7", "6da56a", "8917a0", "8e9c97", "91d4f1", "1.79769313486232E+308", 
                                       "9991d2", "9c0c3a", "KURE5239", "KURE5323", "KURE5650", "KURE5668", 
                                       "KURE5743", "KURE6292", "KURE7339", "KURE7660", "KURE7732", "ad58c9", 
                                       "ba465e", "c49c82", "d2c8f6", "fc6817", "1198b8", "17ec56", "1f3e12", 
                                       "2b5c2d", "2e67bf", "3c17f4", "3ef016", "3fbace", "5b81c7", "6da56a", 
                                       "8917a0", "8e9c97", "91d4f1", "1.79769313486232E+308", "9991d2", 
                                       "9c0c3a", "KURE5239", "KURE5323", "KURE5650", "KURE5668", "KURE5743", 
                                       "KURE6292", "KURE7339", "KURE7660", "KURE7732", "ad58c9", "ba465e", 
                                       "c49c82", "d2c8f6", "fc6817"), run = c(1L, 1L, 1L, 1L, 1L, 1L, 
                                                                              1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 
                                                                              1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 
                                                                              1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 
                                                                              1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 
                                                                              1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 
                                                                              1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 
                                                                              1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 
                                                                              1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 
                                                                              1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 
                                                                              1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 
                                                                              1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 
                                                                              1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 
                                                                              1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 
                                                                              1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 
                                                                              1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 
                                                                              2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 
                                                                              2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 
                                                                              2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 
                                                                              2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 
                                                                              2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 
                                                                              2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 
                                                                              2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 
                                                                              2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 
                                                                              2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 
                                                                              2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 
                                                                              2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 
                                                                              2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 
                                                                              2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 
                                                                              2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 
                                                                              2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L), instruction = c("ignore", 
                                                                                                                                       "ignore", "ignore", "ignore", "ignore", "ignore", "ignore", "ignore", 
                                                                                                                                       "ignore", "ignore", "ignore", "ignore", "ignore", "ignore", "ignore", 
                                                                                                                                       "ignore", "ignore", "ignore", "ignore", "ignore", "ignore", "ignore", 
                                                                                                                                       "ignore", "ignore", "ignore", "ignore", "ignore", "ignore", "ignore", 
                                                                                                                                       "ignore", "ignore", "ignore", "ignore", "ignore", "ignore", "ignore", 
                                                                                                                                       "ignore", "ignore", "ignore", "ignore", "ignore", "ignore", "ignore", 
                                                                                                                                       "ignore", "ignore", "ignore", "ignore", "ignore", "ignore", "ignore", 
                                                                                                                                       "ignore", "ignore", "ignore", "ignore", "ignore", "ignore", "ignore", 
                                                                                                                                       "ignore", "ignore", "ignore", "ignore", "ignore", "ignore", "ignore", 
                                                                                                                                       "ignore", "ignore", "ignore", "ignore", "ignore", "ignore", "ignore", 
                                                                                                                                       "ignore", "ignore", "ignore", "ignore", "ignore", "ignore", "ignore", 
                                                                                                                                       "ignore", "ignore", "ignore", "ignore", "ignore", "ignore", "ignore", 
                                                                                                                                       "ignore", "ignore", "ignore", "ignore", "ignore", "ignore", "ignore", 
                                                                                                                                       "ignore", "ignore", "ignore", "ignore", "ignore", "ignore", "ignore", 
                                                                                                                                       "ignore", "ignore", "ignore", "ignore", "ignore", "ignore", "ignore", 
                                                                                                                                       "ignore", "ignore", "ignore", "ignore", "ignore", "ignore", "ignore", 
                                                                                                                                       "ignore", "ignore", "ignore", "ignore", "ignore", "ignore", "ignore", 
                                                                                                                                       "learn", "learn", "learn", "learn", "learn", "learn", "learn", 
                                                                                                                                       "learn", "learn", "learn", "learn", "learn", "learn", "learn", 
                                                                                                                                       "learn", "learn", "learn", "learn", "learn", "learn", "learn", 
                                                                                                                                       "learn", "learn", "learn", "learn", "learn", "learn", "learn", 
                                                                                                                                       "learn", "learn", "learn", "learn", "learn", "learn", "learn", 
                                                                                                                                       "learn", "learn", "learn", "learn", "learn", "learn", "learn", 
                                                                                                                                       "learn", "learn", "learn", "learn", "learn", "learn", "learn", 
                                                                                                                                       "learn", "learn", "learn", "learn", "learn", "learn", "learn", 
                                                                                                                                       "learn", "learn", "learn", "learn", "learn", "learn", "learn", 
                                                                                                                                       "learn", "learn", "learn", "learn", "learn", "learn", "learn", 
                                                                                                                                       "learn", "learn", "learn", "learn", "learn", "learn", "learn", 
                                                                                                                                       "learn", "learn", "learn", "learn", "learn", "learn", "learn", 
                                                                                                                                       "learn", "learn", "learn", "learn", "learn", "learn", "learn", 
                                                                                                                                       "learn", "learn", "learn", "learn", "learn", "learn", "learn", 
                                                                                                                                       "learn", "learn", "learn", "learn", "learn", "learn", "learn", 
                                                                                                                                       "learn", "learn", "learn", "learn", "learn", "learn", "learn", 
                                                                                                                                       "learn", "learn", "learn", "learn", "learn", "learn", "learn", 
                                                                                                                                       "learn", "ignore", "ignore", "ignore", "ignore", "ignore", "ignore", 
                                                                                                                                       "ignore", "ignore", "ignore", "ignore", "ignore", "ignore", "ignore", 
                                                                                                                                       "ignore", "ignore", "ignore", "ignore", "ignore", "ignore", "ignore", 
                                                                                                                                       "ignore", "ignore", "ignore", "ignore", "ignore", "ignore", "ignore", 
                                                                                                                                       "ignore", "ignore", "ignore", "ignore", "ignore", "ignore", "ignore", 
                                                                                                                                       "ignore", "ignore", "ignore", "ignore", "ignore", "ignore", "ignore", 
                                                                                                                                       "ignore", "ignore", "ignore", "ignore", "ignore", "ignore", "ignore", 
                                                                                                                                       "ignore", "ignore", "ignore", "ignore", "ignore", "ignore", "ignore", 
                                                                                                                                       "ignore", "ignore", "ignore", "ignore", "ignore", "ignore", "ignore", 
                                                                                                                                       "ignore", "ignore", "ignore", "ignore", "ignore", "ignore", "ignore", 
                                                                                                                                       "ignore", "ignore", "ignore", "ignore", "ignore", "ignore", "ignore", 
                                                                                                                                       "ignore", "ignore", "ignore", "ignore", "ignore", "ignore", "ignore", 
                                                                                                                                       "ignore", "ignore", "ignore", "ignore", "ignore", "ignore", "ignore", 
                                                                                                                                       "ignore", "ignore", "ignore", "ignore", "ignore", "ignore", "ignore", 
                                                                                                                                       "ignore", "ignore", "ignore", "ignore", "ignore", "ignore", "ignore", 
                                                                                                                                       "ignore", "ignore", "ignore", "ignore", "ignore", "ignore", "ignore", 
                                                                                                                                       "ignore", "ignore", "ignore", "ignore", "ignore", "ignore", "ignore", 
                                                                                                                                       "ignore", "ignore", "learn", "learn", "learn", "learn", "learn", 
                                                                                                                                       "learn", "learn", "learn", "learn", "learn", "learn", "learn", 
                                                                                                                                       "learn", "learn", "learn", "learn", "learn", "learn", "learn", 
                                                                                                                                       "learn", "learn", "learn", "learn", "learn", "learn", "learn", 
                                                                                                                                       "learn", "learn", "learn", "learn", "learn", "learn", "learn", 
                                                                                                                                       "learn", "learn", "learn", "learn", "learn", "learn", "learn", 
                                                                                                                                       "learn", "learn", "learn", "learn", "learn", "learn", "learn", 
                                                                                                                                       "learn", "learn", "learn", "learn", "learn", "learn", "learn", 
                                                                                                                                       "learn", "learn", "learn", "learn", "learn", "learn", "learn", 
                                                                                                                                       "learn", "learn", "learn", "learn", "learn", "learn", "learn", 
                                                                                                                                       "learn", "learn", "learn", "learn", "learn", "learn", "learn", 
                                                                                                                                       "learn", "learn", "learn", "learn", "learn", "learn", "learn", 
                                                                                                                                       "learn", "learn", "learn", "learn", "learn", "learn", "learn", 
                                                                                                                                       "learn", "learn", "learn", "learn", "learn", "learn", "learn", 
                                                                                                                                       "learn", "learn", "learn", "learn", "learn", "learn", "learn", 
                                                                                                                                       "learn", "learn", "learn", "learn", "learn", "learn", "learn", 
                                                                                                                                       "learn", "learn", "learn", "learn", "learn", "learn", "learn", 
                                                                                                                                       "learn", "learn", "learn"), alpha = c(1, 1, 1, 1, 1, 1, 1, 1, 
                                                                                                                                                                             1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
                                                                                                                                                                             1, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 
                                                                                                                                                                             0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 
                                                                                                                                                                             0.8, 0.8, 0.8, 0.8, 0.8, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 
                                                                                                                                                                             0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 
                                                                                                                                                                             0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 0.4, 0.4, 0.4, 0.4, 
                                                                                                                                                                             0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 
                                                                                                                                                                             0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 
                                                                                                                                                                             1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
                                                                                                                                                                             1, 1, 1, 1, 1, 1, 1, 1, 1, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 
                                                                                                                                                                             0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 
                                                                                                                                                                             0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.6, 0.6, 0.6, 
                                                                                                                                                                             0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 
                                                                                                                                                                             0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 
                                                                                                                                                                             0.6, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 
                                                                                                                                                                             0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 
                                                                                                                                                                             0.4, 0.4, 0.4, 0.4, 0.4, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
                                                                                                                                                                             1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0.8, 0.8, 
                                                                                                                                                                             0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 
                                                                                                                                                                             0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 
                                                                                                                                                                             0.8, 0.8, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 
                                                                                                                                                                             0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 
                                                                                                                                                                             0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 
                                                                                                                                                                             0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 
                                                                                                                                                                             0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 1, 1, 1, 1, 
                                                                                                                                                                             1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
                                                                                                                                                                             1, 1, 1, 1, 1, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 
                                                                                                                                                                             0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 
                                                                                                                                                                             0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 
                                                                                                                                                                             0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 
                                                                                                                                                                             0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 0.4, 0.4, 
                                                                                                                                                                             0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 
                                                                                                                                                                             0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 
                                                                                                                                                                             0.4, 0.4), steadystate = c(16.94283001, 18.71764142, 13.55678726, 
                                                                                                                                                                                                        11.29877377, 14.9199573, 15.95501601, 2.476183473, 1.839531883, 
                                                                                                                                                                                                        19.27444296, 19.96623724, 15.71835112, 11.47072305, 10.66748226, 
                                                                                                                                                                                                        17.57569654, 20.21346018, 8.789130522, 22.29913118, 15.63431345, 
                                                                                                                                                                                                        13.88200922, 17.50973223, 16.19583125, 5.217056822, 19.2028122, 
                                                                                                                                                                                                        13.42813117, 7.908435349, 14.53951637, 8.352761625, 3.151190433, 
                                                                                                                                                                                                        11.59331603, 7.945528305, 0.117097944, 11.25709463, 16.55332944, 
                                                                                                                                                                                                        12.63420524, 15.20997688, 10.24685029, -2.119677231, 4.466024807, 
                                                                                                                                                                                                        8.561176625, 11.96532008, 17.0320533, 19.31705649, 7.923536871, 
                                                                                                                                                                                                        11.83678223, 12.79270191, 13.38649078, 11.15017395, 13.44750397, 
                                                                                                                                                                                                        8.818167294, 14.74830147, -5.564496791, 10.89509052, -2.996518158, 
                                                                                                                                                                                                        17.96814766, 3.011098252, 14.01177335, -0.406612617, -0.211781521, 
                                                                                                                                                                                                        18.21827265, 12.49708134, 16.61597634, 17.28547244, 24.73366336, 
                                                                                                                                                                                                        22.05910344, 23.63287513, 22.37917653, 0.625630211, 3.051846793, 
                                                                                                                                                                                                        26.26188815, 20.90060274, 18.42112024, 19.13380926, 11.45631155, 
                                                                                                                                                                                                        24.88382222, 27.46005811, 13.75928468, 18.64064777, 26.24695211, 
                                                                                                                                                                                                        10.30172419, 26.63700808, 22.03332868, 6.958212244, 13.96331451, 
                                                                                                                                                                                                        20.45020134, 12.8691025, 21.83650855, 12.89197107, 2.75044341, 
                                                                                                                                                                                                        26.19235509, 1.856464162, -1.166229405, -1.366821493, 20.7329696, 
                                                                                                                                                                                                        3.830704344, 15.18342822, 20.02463338, -3.919103813, 0.387268685, 
                                                                                                                                                                                                        29.10752296, 21.73922935, 26.8577648, 23.57667655, 10.47374199, 
                                                                                                                                                                                                        20.56274024, 28.70517324, 29.41537157, 17.2094759, 22.26298021, 
                                                                                                                                                                                                        6.490502092, 21.41986315, -21.86834167, 14.35954997, -6.855000223, 
                                                                                                                                                                                                        30.32611669, 12.19063714, -10.68102404, -3.571294055, 2.346466471, 
                                                                                                                                                                                                        31.99185384, 6.121739813, 11.84010213, 12.28448377, 17.08788547, 
                                                                                                                                                                                                        15.72800772, 16.54643472, 13.319298, 13.93781336, 18.94872071, 
                                                                                                                                                                                                        11.60928855, 7.882863714, 15.55188903, 11.23383767, 15.69289702, 
                                                                                                                                                                                                        17.99542843, 15.46380625, 19.68868349, 17.5328802, 15.20246811, 
                                                                                                                                                                                                        13.46462161, 14.44776924, 12.74633913, 14.11119301, 17.66507963, 
                                                                                                                                                                                                        20.19353363, 15.25350401, 16.16471218, 15.5216436, 19.0945987, 
                                                                                                                                                                                                        15.76670904, 14.52867596, 14.3155334, 11.75079254, 12.34518878, 
                                                                                                                                                                                                        12.91962216, 19.0522722, 17.66871446, 16.58558362, 19.88027568, 
                                                                                                                                                                                                        17.1455929, 15.39200546, 17.45014689, 16.41852702, 13.44456154, 
                                                                                                                                                                                                        11.03333314, 13.3604967, 18.51688111, 20.05335134, 15.41957753, 
                                                                                                                                                                                                        0.379551537, 19.46327023, -9.997968992, 18.36976832, -9.42232179, 
                                                                                                                                                                                                        15.54703177, 14.74842301, 10.26716262, 22.27114943, 14.91204498, 
                                                                                                                                                                                                        21.34489926, 14.05473634, 22.04633751, 26.15272363, 27.26852261, 
                                                                                                                                                                                                        21.26569169, 30.3743437, 21.3080857, 22.42736595, 29.01721269, 
                                                                                                                                                                                                        27.73341336, 11.37741946, 31.63067859, 12.87032661, 27.79529198, 
                                                                                                                                                                                                        23.66177896, 29.48034318, 15.85143084, 22.39957063, 25.99534113, 
                                                                                                                                                                                                        22.57683009, 31.97543418, 15.36527152, 20.89119234, 3.630910927, 
                                                                                                                                                                                                        27.15304522, 21.08700169, 22.52482825, 23.01808274, 26.23928184, 
                                                                                                                                                                                                        25.52577447, 27.09346334, 26.85693072, 8.181883513, 27.17200251, 
                                                                                                                                                                                                        30.46260152, 38.21370942, 24.29303062, 35.30296935, 42.43464662, 
                                                                                                                                                                                                        43.97040368, 26.35621314, 36.39557554, 29.86804554, 25.84531592, 
                                                                                                                                                                                                        22.48362526, 32.19159441, 33.12801687, 17.44484369, 20.5860271, 
                                                                                                                                                                                                        0.301894576, 21.52218172, 18.7599191, 29.16020525, -12.85235031, 
                                                                                                                                                                                                        31.13205292, 21.93102664, -12.36293565, 37.28398064, 34.36900628, 
                                                                                                                                                                                                        34.96540588, 35.50981976, 16.42060244, 4.213594976, 15.12968897, 
                                                                                                                                                                                                        21.75399309, 16.5929969, 12.75086628, 8.708767081, 7.481156852, 
                                                                                                                                                                                                        15.14581805, 11.54300471, 3.629905554, 13.97774937, 10.66748226, 
                                                                                                                                                                                                        11.30181334, 18.13200937, 16.54939126, 12.25357043, 15.0667805, 
                                                                                                                                                                                                        3.482697303, 14.53734189, 14.23426164, 10.53941347, 11.71760607, 
                                                                                                                                                                                                        14.05700837, 9.123839767, 12.53289507, 15.3845097, 7.466340328, 
                                                                                                                                                                                                        14.41898346, 3.872288895, 3.317437743, 7.527552777, 14.12165566, 
                                                                                                                                                                                                        4.338349604, 8.764504871, 13.25952801, -9.654224756, 1.049234356, 
                                                                                                                                                                                                        10.03630807, 9.919641297, 6.687899971, 25.67285482, 7.923536871, 
                                                                                                                                                                                                        13.41545991, 15.75752135, 5.269692106, 19.1326686, 10.59563703, 
                                                                                                                                                                                                        1.308628654, 12.67464745, -18.99677475, 6.140050144, -9.539347061, 
                                                                                                                                                                                                        13.78469002, 15.72075819, 15.67583853, -11.33863559, -1.584771699, 
                                                                                                                                                                                                        13.69517736, 6.901120065, 10.30786103, 1.119851644, 28.18953394, 
                                                                                                                                                                                                        19.28075539, 12.66757654, 24.96318432, 2.072755108, 10.51696891, 
                                                                                                                                                                                                        13.49439231, 21.19195576, 3.456515061, 19.26192828, 11.45631155, 
                                                                                                                                                                                                        17.50648822, 24.04134737, 16.48231586, 22.34334158, 20.63580029, 
                                                                                                                                                                                                        16.77881789, 23.11958556, 22.76932241, 5.781199041, 1.29748058, 
                                                                                                                                                                                                        25.95657189, 8.430331435, 22.83964855, 2.902318749, 3.051809273, 
                                                                                                                                                                                                        18.4248889, 2.853716594, 4.025745436, 3.815246746, 24.66974232, 
                                                                                                                                                                                                        6.596795392, 18.39078283, 17.10394576, -8.258297254, -3.944765953, 
                                                                                                                                                                                                        11.25743472, 29.15945264, 8.958557286, 34.41374002, 10.47374199, 
                                                                                                                                                                                                        22.20885669, 35.02529908, 10.03763192, 27.75962683, 29.53474284, 
                                                                                                                                                                                                        -4.084680104, 26.84334866, 9.319487082, 7.021535876, -4.646624744, 
                                                                                                                                                                                                        27.21070556, 18.27786546, 15.75033849, 6.774360494, -2.574861378, 
                                                                                                                                                                                                        31.52927968, -3.057030372, 9.080497478, 15.55410149, 13.02310819, 
                                                                                                                                                                                                        16.07259187, 17.85544931, 17.10354385, 18.01627986, 18.04464311, 
                                                                                                                                                                                                        19.38281447, 13.86087405, 14.8822656, 19.13526805, 15.69289702, 
                                                                                                                                                                                                        10.62649009, 18.75585093, 15.35395422, 16.46169645, 15.35589119, 
                                                                                                                                                                                                        14.1919963, 16.29162655, 12.05790807, 14.81641864, 2.58785017, 
                                                                                                                                                                                                        13.25503811, 7.476113117, 21.099951, 8.209652055, 17.38117873, 
                                                                                                                                                                                                        10.05858247, 14.57816039, 6.001910882, 15.52806506, 12.80206205, 
                                                                                                                                                                                                        6.810581494, 19.23902056, 20.32200895, 16.20177825, 20.27502266, 
                                                                                                                                                                                                        17.20438708, 15.81225916, 22.36060705, 16.54363126, 13.44456154, 
                                                                                                                                                                                                        11.82702918, 20.20460595, 21.31905301, 20.67382538, 16.13424557, 
                                                                                                                                                                                                        6.5038401, 16.12935405, -10.31181516, 19.24408709, 4.360125116, 
                                                                                                                                                                                                        15.83352441, 13.27192087, 25.60211946, 21.06091214, 20.61153885, 
                                                                                                                                                                                                        15.94263965, 19.13676449, 20.14122848, 26.7568559, 35.91129324, 
                                                                                                                                                                                                        26.96710694, 25.16974693, 23.35505895, 27.52183384, 29.5585771, 
                                                                                                                                                                                                        31.9305197, 20.77029313, 34.83321144, 17.99575696, 27.79529198, 
                                                                                                                                                                                                        19.98093349, 27.88742986, 26.84156258, 22.57284089, 24.25664243, 
                                                                                                                                                                                                        15.56427476, 20.85828689, 20.72758674, 28.24738072, 2.603312116, 
                                                                                                                                                                                                        25.60515773, 14.6862719, 17.68266166, 18.53014005, 29.55515644, 
                                                                                                                                                                                                        24.02451367, 22.03484091, 33.61903672, 21.38141351, 27.86794125, 
                                                                                                                                                                                                        29.91432573, 37.92207702, 33.20958252, 37.09801402, 44.7680907, 
                                                                                                                                                                                                        35.41058944, 34.75796731, 36.19765913, 32.70380146, 25.84531592, 
                                                                                                                                                                                                        19.79049873, 33.95426072, 39.55276353, 31.30165354, 27.18155246, 
                                                                                                                                                                                                        -4.265294377, 21.94536938, 3.215734805, 34.76075193, -10.11036014, 
                                                                                                                                                                                                        20.32561712, 20.76796234, 27.23508367, 35.40233198, 35.3577519, 
                                                                                                                                                                                                        31.68002334, 36.21036173)), class = "data.frame", row.names = c(NA, 
                                                                                                                                                                                                                                                                        -480L))
  
  return(df)

}


plotRunInstructionInteraction <- function() {
  
  df <- getSteadyStateData()
  
  run <- c()
  instruction <- c()
  
  avg <- c()
  CIlo <- c()
  CIhi <- c()
  
  for (runno in c(1,2)) {
    for (instr in c('ignore','learn')) {
      
      sdf <- df[which(df$run == runno & df$instruction == instr),]
      
      run <- c(run, runno)
      instruction <- c(instruction, instr)
      
      avg <- c(avg, mean(sdf$steadystate))
      CI <- Reach::getConfidenceInterval(data=sdf$steadystate)
      
      CIlo <- c(CIlo, min(CI))
      CIhi <- c(CIhi, max(CI))
      
    }
  }
  
  colors <- getColors()
  
  ndf <- data.frame(run, instruction, avg, CIlo, CIhi)
  
  print(ndf)
  
  
  plot(-1000,-1000,
       main='',xlab='instruction',ylab='steady state [°]',
       xlim=c(0.75,2.25),ylim=c(0,25),
       ax=F,bty='n')
  
  for (runno in c(1,2)) {
    
    idx <- which(ndf$run == runno)
    X <- c(1,2,2,1)
    Y <- c(ndf$CIlo[idx],rev(ndf$CIhi[idx]))
    
    polygon( x = X,
             y = Y,
             border = NA,
             col=colors$tr[runno])
    
  }
  
  for (runno in c(1,2)) {
    
    idx <- which(ndf$run == runno)
    lines( x = c(1,2),
           y = ndf$avg[idx],
           col = colors$op[runno] )
    
  }
  
  axis( side = 1, at = c(1,2), labels = c('ignore', 'learn'))
  axis( side = 2, at = seq(0,25,5))
  
  legend( x = 1,
          y = 25,
          legend = c('run 1', 'run 2'),
          lwd = 1,
          col = colors$op[c(1,2)],
          bty='n')
  
}


plotAlphaInstructionInteraction <- function() {
  
  df <- getSteadyStateData()
  
  alpha <- c()
  instruction <- c()
  
  avg <- c()
  CIlo <- c()
  CIhi <- c()
  
  for (alphano in c(1:4)) {
    alpha_level = c(1.0,0.8,0.6,0.4)[alphano]
    for (instr in c('ignore','learn')) {
      
      sdf <- df[which(df$alpha == alpha_level & df$instruction == instr),]
      
      alpha <- c(alpha, alpha_level)
      instruction <- c(instruction, instr)
      
      avg <- c(avg, mean(sdf$steadystate))
      CI <- Reach::getConfidenceInterval(data=sdf$steadystate)
      
      CIlo <- c(CIlo, min(CI))
      CIhi <- c(CIhi, max(CI))
      
    }
  }
  
  colors <- getColors()
  
  ndf <- data.frame(alpha, instruction, avg, CIlo, CIhi)
  
  # print(ndf)
  
  
  plot(-1000,-1000,
       main='',xlab='alpha',ylab='steady state [°]',
       xlim=c(0.2,1.2),ylim=c(0,40),
       ax=F,bty='n')
  
  lines(x=c(0.4,1),y=c(15,15),col='#000000',lty=2)
  
  lines(x=seq(0.4,1,0.05),y=15/seq(0.4,1,0.05),col='#000000',lty=2)
  
  alphalevels <- c(1.0,0.8,0.6,0.4)
  
  for (instr_no in c(1,2)) {
    
    instr <- c('ignore','learn')[instr_no]
    
    idx <- which(ndf$instruction == instr)
    X <- c(alphalevels, rev(alphalevels))
    Y <- c(ndf$CIlo[idx],rev(ndf$CIhi[idx]))
    
    polygon( x = X,
             y = Y,
             border = NA,
             col=colors$tr[instr_no])
    
  }
  
  for (instr_no in c(1,2)) {
    
    instr <- c('ignore','learn')[instr_no]
    
    idx <- which(ndf$instruction == instr)
    lines( x = alphalevels,
           y = ndf$avg[idx],
           col = colors$op[instr_no] )
    
  }
  
  axis( side = 1, at = alphalevels)
  axis( side = 2, at = seq(0,40,5))
  
  legend( x = 0.8,
          y = 30,
          legend = c('ignore', 'learn'),
          lwd = 1,
          col = colors$op[c(1,2)],
          bty='n')
  
}

