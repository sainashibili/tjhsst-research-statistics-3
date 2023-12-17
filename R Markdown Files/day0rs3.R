#rs3 day 0

#vectors most basic data type in R; essentially a list; can take on different types

#character vector, length 1
character <- "abc"

#numeric vector, length 1
numeric <- 12.5

#logical vector, length 1
logical <- TRUE

#str() allows us to see each vector type
str(character)
str(numeric)
str(logical)

#to list out many entries and put them in a vector object use c(); c stands for concatenate/combine
friend_names <- c("Abram", "Bryant", "Colleen", "David", "Esther", "Jeremiah")

#use class function to see what type of an object smth is
class(friend_names)

#seq() function allows us to enter a list of numbers in a range/repeating order
sequence_by_2 <- seq(from = 0, to = 100, by = 2)
dec_frac_seq <- seq(from = 10, to = 3, by = -0.2)

#: operator also does the same thing as seq() but in a consecutive increasing or decreasing range
inc_seq <- 98:112

#can operate with vectors, this next line will add five to every number in sequence by two
sequence_by_2_plus_4 <- sequence_by_2 + 4

#use brackets to select from a list
friend_names[1]
friend_names[2:4]
#every other element
friend_names[seq(1,6, by=2)]
#every element but the 2nd to 4th element
friend_names[-c(2:4)]
#specifying which ones to return
friend_names[c(TRUE, FALSE, FALSE, FALSE, FALSE, TRUE)]

#boolean operator, will return a list that looks like "TRUE FALSE FALSE FALSE FALSE FALSE"
friend_names == "Abram"

#looks through friend_names and returns the entries that match with the arguments given
friend_names[friend_names %in% c("Abram", "Microsoft Bing")]

