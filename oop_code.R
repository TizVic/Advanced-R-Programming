# Coursera - Advanced R programming course
# Part 2: Longitudinal Data Class and Methods
# S4 Classes for Longitudinal data assignment

#################################################################
##      Structure of S4 classes
#################################################################

#################################################################
# Class Name    # Method Name   # Input         # Output        #
#################################################################
# LongitudinalData      # print         # R Console             #
# LongitudinalData      # subject       # Subject class         #
# Subject               # print         # R Console             #
# Subject               # summary       # Summary class         #
# Subject               # visit         # Subject class         #
# Subject               # room          # Room class            #
# Summary               # print         # R Console             #
# Room                  # print         # R Console             #
# Room                  # summary       # Summary class         #
#################################################################


# Loading libraries used in methods
library(dplyr)
library(tidyr)


# Class: Room
## Room: Definition
setClass(
        "Room",
        slots = list(
                idSubject = "integer",
                chRoom = "character",
                rmData = "data.frame"
        ),
        validity = function(object) {
                if (!is.data.frame(object@rmData) || 
                    !is.integer(object@idSubject) || 
                    !is.character(object@chRoom)) {
                        return("Input data is NOT valid!")
                }
                return(TRUE)        
        }
)

## Room: Methods

### Method: print
setMethod(
        f = "print",
        signature = "Room",
        definition = function(x) {
                myVisit = as.integer(unique(x@rmData["visit"]))
                myRoom = as.character(unique(x@rmData["room"])) 
                cat("ID:", x@idSubject, "\n")
                cat("Visit:", myVisit, "\n")
                cat("Room:", myRoom, "\n")
                invisible(x)
        }
)

### Method: summary
setMethod(
        f = "summary",
        signature = "Room",
        definition = function(object) {
                myVisit = as.integer(unique(object@rmData["visit"]))
                myRoom = as.character(unique(object@rmData["room"]))
                z <- object@rmData %>% select(value) %>% 
                        summary() %>% as.data.frame()
                output <- as.data.frame(z[,3])
                names(output) <- NULL
                return(
                        new(
                                "Summary",
                                id = object@idSubject,
                                smData = output
                        )
                )
        }
)

# Class: Subject
## Subject: Definition
setClass(
        # name
        Class = "Subject",
        slots = list(
                sbData = "data.frame",
                id = "integer"
        ),
        validity = function(object) {
                if (!is.data.frame(object@sbData) || !is.integer(object@id)) {
                        return("Input data is NOT valid!")
                }
                return(TRUE)    
        }
)


## Subject: Methods

### method: print
setMethod(
        f = "print",
        signature = "Subject",
        definition = function(x) {
                if (!is.null(x)) {
                        cat("Subject ID:", x@id)
                } else {
                        NULL
                }
                invisible(x)
        }
)

### Method: summary
setMethod(
        f = "summary",
        signature = "Subject",
        definition = function(object) {
                zData <- object@sbData %>% group_by(visit, room) %>% 
                                summarise(value = mean(value)) %>% 
                                spread(room, value) %>% as.data.frame()
                zId <- object@id
                z <- new(
                        "Summary",
                        id = zId,
                        smData = zData
                )
                
                return(z)
                
        }
)

### Method: visit
setGeneric(
        "visit",
        def = function(sbObject, idVisit) {
                standardGeneric("visit")
        }
)
setMethod(
        f = "visit",
        signature = "Subject",
        definition = function(sbObject, idVisit) {
                z <- sbObject@sbData %>% 
                        filter(visit == idVisit) %>%
                        as.data.frame()
                return(new(
                        "Subject",
                        sbData = z,
                        id = sbObject@id
                ))
        }
)

### Method: room 
setGeneric(
        "room",
        def = function(sbObject, chRoom) {
                standardGeneric("room")
        }
)
setMethod(
        f = "room",
        signature = "Subject",
        definition = function(sbObject, chRoom) {
                z <- sbObject@sbData %>% 
                        filter(room == chRoom) %>%
                        as.data.frame()
                return(new(
                        "Room",
                        rmData = z,
                        idSubject = sbObject@id,
                        chRoom = chRoom
                ))
        }
)

# Class: Summary
## Summary: Definition
setClass(
        "Summary",
        slots = c(
                id = "integer",
                smData = "data.frame"
        ),
        validity = function(object) {
                if (!is.data.frame(object@smData) || !is.integer(object@id)) {
                        return("Input data is NOT valid!")
                }
                return(TRUE)        
        }
        
)

## Summary: Methods

### Method: print
setMethod(
        f = "print",
        signature = "Summary",
        definition = function(x) {
                if (!is.null(x)) {
                        cat("ID:", x@id, "\n")
                        print(as.data.frame(x@smData))
                        
                } else {
                        NULL
                }
                invisible(x)
        }
)


# Class: LongitudinalData
## LongitudinalData: Definition
setClass(
        # Name of class
        "LongitudinalData",
        
        # Slots
        slots =  c(
                ldData = "data.frame"
        ),
        
        # Test if data is consistent
        validity = function(object) {
                if (!is.data.frame(object@ldData)) {
                        return("Input data is NOT a data frame")
                }
                return(TRUE)        
        }
)

## LongitudinalData: Methods

### Method: print
setMethod(
        f = "print",
        signature = "LongitudinalData",
        definition = function(x) {
                  cat("Longitudinal dataset with ",
                      nrow(unique(x@ldData["id"])),
                      "subjects\n")
                  invisible(x)
                
        }
)

### Method: subject
setGeneric(
        name = "subject",
        def = function(ldObject, idSubject) {
                standardGeneric("subject")
        }
)
setMethod(
        f = "subject",
        signature = "LongitudinalData",
        definition = function(ldObject, idSubject) {
                z <- ldObject@ldData %>%  filter(id==idSubject)
                if (is.data.frame(z) && nrow(z)!=0) {
                        z1 <- new("Subject", sbData = z, id = as.integer(idSubject))
                        return(z1)
                } else {
                        return(NULL)
                }
        }
)

# Helper function: make_LD
make_LD <- function(myDataFrame) {
        #        z <- x %>% group_by(id, visit, room)
        myData <- new(
                Class = "LongitudinalData",
                ldData = as.data.frame(myDataFrame)
        )
}

