## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setupDarwin, include=FALSE, eval = Sys.info()[["sysname"]] == "Darwin"----
#The following line seems to be required by pkgdown::build_site() on my machine, but causes build to break with R-CMD-CHECK on GH
knitr::opts_chunk$set(dev = "png", dev.args = list(type = "cairo-png"))

## ----message=FALSE------------------------------------------------------------
library(Rcompadre)
library(dplyr)
library(ggplot2)
library(maps)     # for plotting world map
library(popbio)   # for calculating population growth rates

## ----results=FALSE------------------------------------------------------------
y <- c(0.2, 4.1, 3.7)

mean(y)         # 'normal' expression
y %>% mean()    # piped expression

## ----results=FALSE------------------------------------------------------------
y %>% mean()       # dot is implicit
y %>% mean(x = .)  # dot is explicit

## ----results=FALSE------------------------------------------------------------
x <- 1:3
y %>% data.frame(col1 = x, col2 = .) # use dot to pass object to second argument

## ----eval=FALSE---------------------------------------------------------------
#  # approach 1 (nested functions)
#  compadre_use <- subset(cdb_flag(Compadre), check_NA_A == FALSE)
#  
#  # approach 2 (intermediate step)
#  compadre_flag <- cdb_flag(Compadre)
#  compadre_use <- subset(compadre_flag, check_NA_A == FALSE)

## -----------------------------------------------------------------------------
compadre_use <- Compadre %>% 
  cdb_flag() %>%               # first argument is Compadre, from previous line
  subset(check_NA_A == FALSE)  # first argument is output of cdb_flag()

## ---- warning=FALSE-----------------------------------------------------------
compadre_euro <- Compadre %>%
  subset(Continent == "Europe") %>% 
  mutate(Nordic = Country %in% c("NOR", "SWE", "DNK", "ISL", "FIN"))

## -----------------------------------------------------------------------------
compadre_use <- Compadre %>% 
  mutate(has_active = mpm_has_active(.)) %>% 
  subset(has_active == TRUE) %>% 
  mutate(StudyID = cdb_id_studies(.))

## -----------------------------------------------------------------------------
compadre_unnest <- Compadre %>% 
  mutate(mat_U = matU(.),
         m_class_organized = MatrixClassOrganized(.))

## -----------------------------------------------------------------------------
compadre_lambda <- Compadre %>% 
  cdb_flag() %>% 
  subset(check_NA_A == FALSE) %>%      # remove matrices with missing values
  mutate(mat_A = matA(.)) %>%          # extract list-column of matA
  mutate(lam = sapply(mat_A, lambda))  # apply lambda() to every matA

## -----------------------------------------------------------------------------
compadre_stage_surv <- Compadre %>% 
  mutate(stage_survival = lapply(matU(.), colSums))

# print vector of stage-specific survival for 20th row
compadre_stage_surv$stage_survival[[20]]

## -----------------------------------------------------------------------------
SurvFirstActive <- function(matU, first_active) colSums(matU)[first_active]

## -----------------------------------------------------------------------------
compadre_surv_first_active <- Compadre %>% 
  mutate(surv_1 = mapply(FUN = SurvFirstActive,              # function
                         matU = matU(.),                     # argument 1
                         first_active = mpm_first_active(.)) # argument 2
         )

## -----------------------------------------------------------------------------
# count number of unique populations by species
Compadre %>% 
  group_by(SpeciesAccepted) %>% 
  summarize(n_populations = length(unique(MatrixPopulation))) %>% 
  arrange(desc(n_populations)) # arrange in descending order of n_pops

## -----------------------------------------------------------------------------
# subset to species with 10+ unique populations
compadre_replicated_pops <- Compadre %>% 
  group_by(SpeciesAccepted) %>% 
  mutate(n_pops = length(unique(MatrixPopulation))) %>% 
  ungroup() %>%
  subset(n_pops >= 10)

## -----------------------------------------------------------------------------
singleRepresentativeSpecies <- Compadre %>% 
  group_by(SpeciesAccepted) %>% 
  slice(sample(1))

## ----warning=FALSE, fig.width = 6, fig.height = 4-----------------------------
ggplot(Compadre, aes(Lon, Lat)) +
  borders(database = "world", fill = "grey80", col = NA) +
  geom_point(col = "steelblue", size = 1.8, alpha = 0.8)

## ----warning=FALSE, fig.width = 6, fig.height = 4-----------------------------
# function to calculate life expectancy
lifeExpectancy <- function(matU, startLife) {
  N <- solve(diag(nrow(matU)) - matU)
  return(colSums(N)[startLife])
}

compadre_life_expect <- Compadre %>%
  filter(MatrixComposite != "Seasonal", # filter is the dplyr version of subset
         MatrixTreatment == "Unmanipulated",
         MatrixCaptivity == "W",
         AnnualPeriodicity == "1") %>% 
  mutate(StageID = cdb_id_stages(.)) %>%
  cdb_collapse(columns = "StageID") %>%
  cdb_flag() %>% 
  filter(check_NA_U == FALSE,
         check_zero_U == FALSE,
         check_singular_U == FALSE) %>% 
  mutate(matU = matU(.), start_life = mpm_first_active(.)) %>% 
  mutate(life_expectancy = mapply(lifeExpectancy, matU, start_life)) %>% 
  filter(life_expectancy >= 1) %>% 
  mutate(OrganismType = reorder(OrganismType, life_expectancy, median))

ggplot(compadre_life_expect, aes(OrganismType, life_expectancy)) +
  geom_boxplot() +
  scale_y_log10() +
  coord_flip() +
  labs(x = NULL, y = "Life expectancy (years)")

