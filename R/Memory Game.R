# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

# Define the player's score and input the data

score <- 0
words <- c("quintessential", "dog", "cat", "bark", "green", "red", "table")
answer <- 0


# Instruction phrase
print("Please memorize the following words:")
print(sample(words, 3))

# Asking for memorized words
while (answer != 1) {
  answer <- readline("Please type the words that you remember, and press enter after each word.
         Type 1 and press enter when you are finished.")
}

# Assign points


# Compare points to previous players









