library(tidyverse)
library(stringr)
library(base64enc)

decoding_google_form <- function(path_to_file) {
  
  data = read.csv(path_to_file)
  decoded <- list()
  
  for(i in 1:nrow(data)){
    decoded[[i]] <- suppressMessages(
      read_csv(
        rawToChar(
          base64decode(
            as.character(data[i,3])))))
    decoded[[i]]$email <- data[i,2] 
  }
  
  result <- as.data.frame(do.call("rbind", decoded))
  attributes(result)$spec <- NULL
  attributes(result)$row.names <- 1:nrow(result)
  class(result$datetime) = c('POSIXt','POSIXct')
  
  return(result)
}

decoded_df <- decoding_google_form("activities/a/HW1/Intro to R - Homework Submission (Responses) - Form Responses 1.csv")

decoded_df$user[which(decoded_df$user == "ClaraHibbs")] <- "Clara Hibbs"
decoded_df$user[which(decoded_df$user == "Trinity")] <- "Trinity Williford"

filt_decoded_allqs_df <- decoded_df %>% filter(user != "Eleana") %>% arrange(user, course_name)

filt_decoded_df <- decoded_df |> filter(user != "Eleana", is.na(correct)) %>% arrange(user, course_name)

lesson_counts <- aggregate(
  data = filt_decoded_df,
  lesson_name ~ user,
  FUN = function(x) length(unique(x))
)

write_csv(filt_decoded_allqs_df, 'IntroToR-SubmissionsDecoded-AllQs-07072025.csv')
write_csv(filt_decoded_df, 'IntroToR-SubmissionsDecoded-CompleteQ-07072025.csv')
write_csv(lesson_counts, 'IntroToR-SubmissionsDecoded-LessonCountsPerStudent-07072025.csv')



# Plot HW1

filt_decoded_allqs_df |> 
  filter(course_name == "R Programming") |>
  mutate(lesson_name = fct_inorder(lesson_name)) |>
  ggplot(aes(x = question_number)) + geom_bar() + facet_grid(lesson_name ~ user)

lessons <- c("Principles_of_Analytic_Graphs",
"Exploratory_Graphs",
"Graphics_Devices_in_R",
"Plotting_Systems",
"Base_Plotting_System",
"Lattice_Plotting_System",
"Working_with_Colors",
"GGPlot2_Part1",
"GGPlot2_Part2",
"GGPlot2_Extras")

# Plot HW2
filt_decoded_allqs_df |> 
  filter(course_name == "Exploratory_Data_Analysis") |>
  mutate(lesson_name = factor(lesson_name, levels = lessons)) |>
  ggplot(aes(x = question_number)) + geom_bar() + facet_grid(lesson_name ~ user)

