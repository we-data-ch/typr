source('b_generic_functions.R')
source('c_types.R')
source('a_std.R', echo = FALSE)

df <- data.frame(Training = c('Strength' |> Character(), 'Stamina' |> Character(), 'Other' |> Character()),
 Pulse = c(100L |> Integer(), 150L |> Integer(), 120L |> Integer()),
 Duration = c(60L |> Integer(), 30L |> Integer(), 45L |> Integer())) |> Array0()
df