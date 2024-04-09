library(fixest)
library(stringr)


source("tables_commands.R")

# Load the data
dt<-iris

lm1<-feols(Sepal.Length~Sepal.Width+Petal.Length+Petal.Width,data=dt)
lm2<-feols(Sepal.Length~Sepal.Width+Petal.Length,data=dt)
lm3<-feols(Sepal.Length~Sepal.Width,data=dt)


etable(lm1,lm2,lm3,
       se="hetero",                         # "iid" / "hetero" etc.
       # cluster = ~Species,                    # Clustered SE 
       digits = "r3",                         # rounding - coef/se
       digits.stats = "r2",                   # rounding - fit stat (R^2 etc)
       fitstat=c('r2','n'),                   # Which fit stat to keep?
       dict = c("(Intercept)"="cons"),
       order = c("-cons","Petal","Sepal"),
       style.tex = style.tex("base",line.top = "",line.bottom = "",
                             model.title="",stats.title="",
                             fixef.title = "%",slopes.title = "",fixef.suffix = " FE",fixef.prefix = "%",
                             var.title = "\\midrule \n%Coefficients part", yesNo = "$\\checkmark$"),
       # extralines = list("_^Group"=list("group A"=2,"group B"=1)), # Add extra lines - optional (see help)
       postprocess.tex = mypostprocess,
       # Only one og the two following lines can be used:
       # onlyinner=FALSE,view=TRUE,            # Uncomment for a quick view before compiling
       replace = TRUE,file = "table.tex",   # Uncomment to save the table to a file
       tex = TRUE)


lm1.b<-feols(Sepal.Length~Sepal.Width+Petal.Length+Petal.Width,data=dt,subset = dt$Species=="setosa")
lm2.b<-feols(Sepal.Length~Sepal.Width+Petal.Length,data=dt,subset = dt$Species=="setosa")
lm3.b<-feols(Sepal.Length~Sepal.Width,data=dt,subset = dt$Species=="setosa")

etable(lm1.b,lm2.b,lm3.b,
       se="hetero",                         # "iid" / "hetero" etc.
       # cluster = ~Species,                    # Clustered SE 
       digits = "r3",                         # rounding - coef/se
       digits.stats = "r2",                   # rounding - fit stat (R^2 etc)
       fitstat=c('r2','n'),                   # Which fit stat to keep?
       dict = c("(Intercept)"="cons"),
       order = c("-cons","Petal","Sepal"),
       style.tex = style.tex("base",line.top = "",line.bottom = "",
                             model.title="",stats.title="",
                             fixef.title = "%",slopes.title = "",fixef.suffix = " FE",fixef.prefix = "%",
                             var.title = "\\midrule \n%Coefficients part", yesNo = "$\\checkmark$"),
       # extralines = list("_^Group"=list("group A"=2,"group B"=1)), # Add extra lines - optional (see help)
       postprocess.tex = mypostprocess,
       # Only one og the two following lines can be used:
       # onlyinner=FALSE,view=TRUE,            # Uncomment for a quick view before compiling
       replace = TRUE,file = "table_only_setosa.tex",   # Uncomment to save the table to a file
       tex = TRUE)


merge_tables(file_paths = c("table.tex",
                            "table_only_setosa.tex"),
             labels = c("All Sample","Only Setosa"),
             filename = "table_panels.tex" )

# Optional: remove some lines from the tex file (using regrex)
comment_out_tex_lines(file_path = "table_panels.tex",c("R\\$\\^2\\$","Obs"))
# Optional: uncomment other lines from the tex file (using regrex)
uncomment_tex_lines(file_path = "table_panels.tex",c("Obs"))


