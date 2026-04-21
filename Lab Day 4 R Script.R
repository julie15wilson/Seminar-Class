### Lab Day 4 R Script 

### Start by using packages
library(wooldridge)

install.packages('modelsummary')
install.packages('pandoc')
install.packages('flextable')
install.packages('officer')

library(pandoc) # install package if not already installed by removing the comment above.
library(modelsummary) # Install package if not already installed by removing the comment above.
library(flextable)
library(officer)

## Wooldridge Ch. 13 C7 (i) and (iii)
# Use GPA3 for this exercise. The data set is for 366 student-athletes from a large university
# for fall and spring semesters. Because you have two terms of data for each student, an unobserved 
# effects model is appropriate. The primary question of interest is this: do athletes perform more 
# poorly in school during the semester their sport is in season. 

# (i) Use Pooled OLS to estimate a model with term GPA (trmgpa) as the dependent variable. 
# The explanatory variables are spring, sat, hsperc, female, black, white, frstsem, tothrs, 
# crsgpa, and season. Interpret the coefficient on season. Is it statistically significant? 

# load the data
data(gpa3)

# use linear model
pooled_gpa <- lm(trmgpa ~ spring + sat + hsperc + female + black + white + frstsem + tothrs + crsgpa +season, data = gpa3 )
gpa_output <- summary(pooled_gpa)
print(gpa_output)

# (iii) Use the data differenced across the two terms. Which variables drop out? 
# Now, test for an in-season effect. 

# We need to start by declaring the panel structure of the data and loading the panel data library
library(plm) # load the panel data library
gpa_panel <- pdata.frame(gpa3, index = c("id","term")) # tell the program this is panel data

# Then we use the FD model to run the regression:
gpa_fd_model <- plm(trmgpa ~ spring + sat + hsperc + female + black + white + frstsem + tothrs + crsgpa +season,
                    data = gpa_panel, 
                    model = "fd")
print(summary(gpa_fd_model)) # print output

# Output tables using modelsummary()
tbl <- modelsummary(
  list("Pooled OLS" = pooled_gpa, "First Difference" = gpa_fd_model),
  output = "flextable",  # returns a flextable object instead of saving
  stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  gof_map = c("nobs", "r.squared", "adj.r.squared"),
  estimate  = "{estimate}{stars}",
  statistic = "({std.error})",
  title = "Table 1: Effects of Athletic Season on Student GPA",
  notes = "Standard errors in parentheses. * p<0.1, ** p<0.05, *** p<0.01"
)

# --- Step 1: Add column numbers (1), (2), etc. above model name headers ---
# Find how many model columns there are (excluding the coefficient label column)
n_models <- 2  # change this to match the number of models in your table

col_numbers <- c("", paste0("(", seq_len(n_models), ")"))  # e.g. "", "(1)", "(2)"

tbl <- flextable::set_header_labels(
  tbl,
  values = setNames(
    paste0("(", seq_len(n_models), ")"),
    paste0("Model ", seq_len(n_models))  # must match internal column names
  )
)

# Reorder so numbers appear ABOVE the model names
# ...actually swap rows 1 and 2 of the header:
tbl <- flextable::add_header_row(tbl, values = col_numbers, colwidths = rep(1, n_models + 1), top = TRUE)


# --- Step 2: Horizontal line below Adj. R2 row ---
# Find the row index of Adj. R2 in the body
# modelsummary puts GOF stats at the bottom; with nobs + r.squared + adj.r.squared
# the order is: N, R2, Adj. R2 — so Adj. R2 is the last GOF row
# Count total body rows to find it:
n_rows <- nrow(tbl$body$dataset)
adj_r2_row <- n_rows - 0  # Adj. R2 is the last data row above the note

tbl <- flextable::hline(
  tbl,
  i = adj_r2_row,
  border = fp_border(color = "black", width = 1),
  part = "body"
)

# --- Step 3: Horizontal line below the table note ---
tbl <- flextable::hline_bottom(
  tbl,
  border = fp_border(color = "black", width = 1),
  part = "footer"
)

tbl <- flextable::set_table_properties(
  tbl,
  width = 1,
  layout = "autofit"
)


# --- Save to Word ---
save_as_docx(tbl, path = "C:/Users/julie/OneDrive/Documents/GitHub/Seminar-Class.docx")

# Export to PowerPoint
tbl <- flextable::fit_to_width(tbl, max_width = 9)  # ~9" fits a standard 13.3" widescreen slide
tbl <- flextable::fontsize(tbl, size = 12, part = "all")
save_as_pptx(tbl, path = "gpa_table.pptx")

### END R SCRIPT