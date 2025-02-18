# Load necessary library
library(tibble)

# Create the data frame with political instability and election upsets
state_unrest <- tibble(
  state_name = c(
    "Andhra Pradesh", "Andhra Pradesh",
    "Bihar", "Bihar", "Bihar", "Bihar", "Bihar", "Bihar", "Bihar",
    "Gujarat", "Gujarat",
    "Karnataka", "Karnataka",
    "Madhya Pradesh", "Madhya Pradesh",
    "Uttar Pradesh", "Uttar Pradesh",
    "West Bengal", "West Bengal"
  ),
  Year = c(
    "1994", "2014",
    "1995", "1996", "1997", "1998", "1999", "2000", "2005",
    "1998", "2002",
    "1994", "2008",
    "1993", "2018",
    "1996", "2007",
    "1991", "2011"
  ),
  unrest_event = c(
    "Congress defeated TDP, marking a significant political shift.",
    "TDP gained power post-state bifurcation.",
    "Period marked by deep political divisions and instability.",
    "Period marked by deep political divisions and instability.",
    "Period marked by deep political divisions and instability.",
    "Period marked by deep political divisions and instability.",
    "Period marked by deep political divisions and instability.",
    "Period marked by deep political divisions and instability.",
    "Fractured mandate in February, two elections held in the same year.",
    "Early elections caused by defections and political instability.",
    "BJP’s decisive win post-Gujarat riots, led by Narendra Modi.",
    "Congress regained power.",
    "Early elections due to political instability and President’s Rule.",
    "BJP gained significant ground in state politics.",
    "Congress ended BJP's 15-year rule.",
    "Political instability led to early elections.",
    "BSP secured a decisive majority under Mayawati.",
    "Left Front maintained dominance in the state.",
    "Mamata Banerjee’s TMC ended 34 years of Left Front rule."
  )
)



state_unrest=state_unrest %>% clean_names() %>% 
  mutate(unrest=1)

state_unrest
