# Load necessary library
library(tibble)

# Create the data frame
# Data for Andhra Pradesh Legislative Assembly
state_party1 <- tibble(
  state = c(
    "Andhra Pradesh", "Andhra Pradesh", "Andhra Pradesh", "Andhra Pradesh", "Andhra Pradesh",
    "Andhra Pradesh", "Andhra Pradesh", "Andhra Pradesh", "Andhra Pradesh", "Andhra Pradesh",
    "Andhra Pradesh", "Andhra Pradesh", "Andhra Pradesh", "Andhra Pradesh"
  ),
  assembly_no = c(
    1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14
  ),
  term_duration = c(
    "1956–1962", "1962–1967", "1967–1972", "1972–1978", "1978–1983",
    "1983–1985", "1985–1989", "1989–1994", "1994–1999", "1999–2004",
    "2004–2009", "2009–2014", "2014–2019", "2019–Present"
  ),
  ruling_party = c(
    "Indian National Congress", "Indian National Congress", "Indian National Congress",
    "Indian National Congress", "Indian National Congress", "Telugu Desam Party",
    "Telugu Desam Party", "Indian National Congress", "Telugu Desam Party",
    "Telugu Desam Party", "Indian National Congress", "Indian National Congress",
    "Telugu Desam Party", "Yuvajana Sramika Rythu Congress Party"
  ),
  party_acronym = c(
    "INC", "INC", "INC", "INC", "INC", "TDP", "TDP", "INC", "TDP", "TDP",
    "INC", "INC", "TDP", "YSRCP"
  )
)


# Print the data frame
#state_party1


state_party2 <- tibble( # Data for Arunachal Pradesh Legislative Assembly
     state = c(
      "Arunachal Pradesh", "Arunachal Pradesh", "Arunachal Pradesh", "Arunachal Pradesh", 
      "Arunachal Pradesh", "Arunachal Pradesh", "Arunachal Pradesh", "Arunachal Pradesh", 
      "Arunachal Pradesh", "Arunachal Pradesh"
    ),
    assembly_no = c(
      1, 2, 3, 4, 5, 6, 7, 8, 9, 10
    ),
    term_duration = c(
      "1978–1980", "1980–1984", "1984–1990", "1990–1995", 
      "1995–1999", "1999–2004", "2004–2009", "2009–2014", 
      "2014–2019", "2019–Present"
    ),
    ruling_party = c(
      "Janata Party", "Indian National Congress", "Indian National Congress", 
      "Indian National Congress", "Indian National Congress", 
      "Indian National Congress", "Indian National Congress", 
      "Indian National Congress", "Indian National Congress", "Bharatiya Janata Party"
    ),
    party_acronym = c(
      "JP", "INC", "INC", "INC", "INC", "INC", "INC", "INC", "INC", "BJP"
    )
  )


state_party3 <- tibble(# Data for Assam Legislative Assembly
      state = c(
      "Assam", "Assam", "Assam", "Assam", "Assam",
      "Assam", "Assam", "Assam", "Assam", "Assam",
      "Assam", "Assam", "Assam", "Assam", "Assam"
    ),
    assembly_no = c(
      1, 2, 3, 4, 5, 
      6, 7, 8, 9, 10,
      11, 12, 13, 14, 15
    ),
    term_duration = c(
      "1952–1957", "1957–1962", "1962–1967", "1967–1972", "1972–1978",
      "1978–1983", "1983–1985", "1985–1991", "1991–1996", "1996–2001",
      "2001–2006", "2006–2011", "2011–2016", "2016–2021", "2021–Present"
    ),
    ruling_party = c(
      "Indian National Congress", "Indian National Congress", "Indian National Congress",
      "Indian National Congress", "Indian National Congress", "Janata Party",
      "Indian National Congress", "Indian National Congress", "Indian National Congress",
      "Asom Gana Parishad", "Indian National Congress", "Indian National Congress",
      "Indian National Congress", "Bharatiya Janata Party", "Bharatiya Janata Party"
    ),
    party_acronym = c(
      "INC", "INC", "INC", "INC", "INC", 
      "JP", "INC", "INC", "INC", "AGP",
      "INC", "INC", "INC", "BJP", "BJP"
    )
  )


state_party4 <- tibble(# Data for Bihar Legislative Assembly
    state = c(
      "Bihar", "Bihar", "Bihar", "Bihar", "Bihar",
      "Bihar", "Bihar", "Bihar", "Bihar", "Bihar",
      "Bihar", "Bihar", "Bihar", "Bihar", "Bihar",
      "Bihar", "Bihar"
    ),
    assembly_no = c(
      1, 2, 3, 4, 5, 
      6, 7, 8, 9, 10,
      11, 12, 13, 14, 15,
      16, 17
    ),
    term_duration = c(
      "1952–1957", "1957–1962", "1962–1967", "1967–1969", "1969–1972",
      "1972–1977", "1977–1980", "1980–1985", "1985–1990", "1990–1995",
      "1995–2000", "2000–2005", "2005 (Feb–Oct)", "2005–2010", "2010–2015",
      "2015–2020", "2020–Present"
    ),
    ruling_party = c(
      "Indian National Congress", "Indian National Congress", "Indian National Congress",
      "Indian National Congress", "Indian National Congress", "Indian National Congress",
      "Janata Party", "Indian National Congress", "Indian National Congress",
      "Janata Dal", "Rashtriya Janata Dal", "Rashtriya Janata Dal",
      "President’s Rule", "JD(U)-BJP Coalition", "JD(U)-BJP Coalition",
      "RJD-JD(U) Alliance", "JD(U)-RJD Alliance"
    ),
    party_acronym = c(
      "INC", "INC", "INC", "INC", "INC", 
      "INC", "JP", "INC", "INC", "JD",
      "RJD", "RJD", "PR", "JD(U)-BJP", "JD(U)-BJP",
      "RJD-JD(U)", "JD(U)-RJD"
    )
  )

state_party5 <- tibble(# Data for Gujarat Legislative Assembly
    state = c(
      "Gujarat", "Gujarat", "Gujarat", "Gujarat", "Gujarat",
      "Gujarat", "Gujarat", "Gujarat", "Gujarat", "Gujarat",
      "Gujarat", "Gujarat", "Gujarat"
    ),
    assembly_no = c(
      1, 2, 3, 4, 5,
      6, 7, 8, 9, 10,
      11, 12, 13
    ),
    term_duration = c(
      "1960–1962", "1962–1967", "1967–1971", "1971–1975", "1975–1980",
      "1980–1985", "1985–1990", "1990–1995", "1995–1998", "1998–2002",
      "2002–2007", "2007–2012", "2012–Present"
    ),
    ruling_party = c(
      "Indian National Congress", "Indian National Congress", "Indian National Congress",
      "Indian National Congress", "Indian National Congress", "Indian National Congress",
      "Indian National Congress", "Janata Party", "Bharatiya Janata Party",
      "Bharatiya Janata Party", "Bharatiya Janata Party", "Bharatiya Janata Party",
      "Bharatiya Janata Party"
    ),
    party_acronym = c(
      "INC", "INC", "INC", "INC", "INC", 
      "INC", "INC", "JP", "BJP", "BJP",
      "BJP", "BJP", "BJP"
    )
  )


# Data for Odisha Legislative Assembly
state_party6 <- tibble(
  state = c(
    "Odisha", "Odisha", "Odisha", "Odisha", "Odisha",
    "Odisha", "Odisha", "Odisha", "Odisha", "Odisha",
    "Odisha", "Odisha", "Odisha", "Odisha", "Odisha",
    "Odisha", "Odisha"
  ),
  assembly_no = c(
    1, 2, 3, 4, 5,
    6, 7, 8, 9, 10,
    11, 12, 13, 14, 15,
    16, 17
  ),
  term_duration = c(
    "1952–1957", "1957–1961", "1961–1967", "1967–1971", "1971–1974",
    "1974–1977", "1977–1980", "1980–1985", "1985–1990", "1990–1995",
    "1995–2000", "2000–2004", "2004–2009", "2009–2014", "2014–2019",
    "2019–2024", "2024–Present"
  ),
  ruling_party = c(
    "Indian National Congress", "Indian National Congress", "Indian National Congress",
    "Indian National Congress", "Indian National Congress", "Indian National Congress",
    "Janata Party", "Indian National Congress", "Janata Dal",
    "Janata Dal", "Indian National Congress", "Biju Janata Dal",
    "Biju Janata Dal", "Biju Janata Dal", "Biju Janata Dal",
    "Biju Janata Dal", "To be determined (Upcoming election)"
  ),
  party_acronym = c(
    "INC", "INC", "INC", "INC", "INC",
    "INC", "JP", "INC", "JD", "JD",
    "INC", "BJD", "BJD", "BJD", "BJD",
    "BJD", "TBD"
  )
)


# Data for Haryana Legislative Assembly
state_party7 <- tibble(
  state = c(
    "Haryana", "Haryana", "Haryana", "Haryana", "Haryana",
    "Haryana", "Haryana", "Haryana", "Haryana", "Haryana",
    "Haryana", "Haryana", "Haryana"
  ),
  assembly_no = c(
    1, 2, 3, 4, 5,
    6, 7, 8, 9, 10,
    11, 12, 13
  ),
  term_duration = c(
    "1966–1967", "1967–1968", "1968–1972", "1972–1977", "1977–1982",
    "1982–1987", "1987–1991", "1991–1996", "1996–2000", "2000–2005",
    "2005–2009", "2009–2014", "2014–Present"
  ),
  ruling_party = c(
    "Indian National Congress", "Indian National Congress", "Indian National Congress",
    "Indian National Congress", "Janata Party", "Indian National Congress",
    "Lok Dal", "Indian National Congress", "Indian National Congress",
    "Indian National Congress", "Indian National Congress", "Bharatiya Janata Party",
    "Bharatiya Janata Party"
  ),
  party_acronym = c(
    "INC", "INC", "INC", "INC", "JP", 
    "INC", "LD", "INC", "INC", "INC",
    "INC", "BJP", "BJP"
  )
)


# Data for Himachal Pradesh Legislative Assembly
state_party8 <- tibble(
  state = c(
    "Himachal Pradesh", "Himachal Pradesh", "Himachal Pradesh", "Himachal Pradesh", "Himachal Pradesh",
    "Himachal Pradesh", "Himachal Pradesh", "Himachal Pradesh", "Himachal Pradesh", "Himachal Pradesh",
    "Himachal Pradesh", "Himachal Pradesh", "Himachal Pradesh"
  ),
  assembly_no = c(
    1, 2, 3, 4, 5,
    6, 7, 8, 9, 10,
    11, 12, 13
  ),
  term_duration = c(
    "1952–1957", "1957–1962", "1962–1967", "1967–1972", "1972–1977",
    "1977–1982", "1982–1985", "1985–1990", "1990–1995", "1995–2000",
    "2000–2005", "2005–2012", "2012–Present"
  ),
  ruling_party = c(
    "Indian National Congress", "Indian National Congress", "Indian National Congress",
    "Indian National Congress", "Indian National Congress", "Janata Party",
    "Indian National Congress", "Indian National Congress", "BJP",
    "Indian National Congress", "BJP", "Indian National Congress",
    "Indian National Congress"
  ),
  party_acronym = c(
    "INC", "INC", "INC", "INC", "INC",
    "JP", "INC", "INC", "BJP", "INC",
    "BJP", "INC", "INC"
  )
)

# Data for Karnataka Legislative Assembly
state_party9 <- tibble(
  state = c(
    "Karnataka", "Karnataka", "Karnataka", "Karnataka", "Karnataka",
    "Karnataka", "Karnataka", "Karnataka", "Karnataka", "Karnataka",
    "Karnataka", "Karnataka", "Karnataka", "Karnataka"
  ),
  assembly_no = c(
    1, 2, 3, 4, 5,
    6, 7, 8, 9, 10,
    11, 12, 13, 14
  ),
  term_duration = c(
    "1952–1957", "1957–1962", "1962–1967", "1967–1972", "1972–1977",
    "1977–1983", "1983–1985", "1985–1989", "1989–1994", "1994–1999",
    "1999–2004", "2004–2008", "2008–2013", "2013–Present"
  ),
  ruling_party = c(
    "Indian National Congress", "Indian National Congress", "Indian National Congress",
    "Indian National Congress", "Indian National Congress", "Janata Party",
    "Janata Dal", "Indian National Congress", "Janata Dal", "Indian National Congress",
    "Indian National Congress", "Bharatiya Janata Party", "Bharatiya Janata Party",
    "Indian National Congress"
  ),
  party_acronym = c(
    "INC", "INC", "INC", "INC", "INC",
    "JP", "JD", "INC", "JD", "INC",
    "INC", "BJP", "BJP", "INC"
  )
)



# Data for Kerala Legislative Assembly
state_party10 <- tibble(
  state = c(
    "Kerala", "Kerala", "Kerala", "Kerala", "Kerala",
    "Kerala", "Kerala", "Kerala", "Kerala", "Kerala",
    "Kerala", "Kerala", "Kerala", "Kerala"
  ),
  assembly_no = c(
    1, 2, 3, 4, 5,
    6, 7, 8, 9, 10,
    11, 12, 13, 14
  ),
  term_duration = c(
    "1957–1959", "1960–1965", "1965–1970", "1970–1977", "1977–1980",
    "1980–1982", "1982–1987", "1987–1991", "1991–1996", "1996–2001",
    "2001–2006", "2006–2011", "2011–2016", "2016–Present"
  ),
  ruling_party = c(
    "Communist Party of India", "Indian National Congress", "Indian National Congress",
    "Indian National Congress", "Indian National Congress", "Indian National Congress",
    "Indian National Congress", "Communist Party of India (Marxist)", "Indian National Congress",
    "Communist Party of India (Marxist)", "Indian National Congress", "Communist Party of India (Marxist)",
    "Communist Party of India (Marxist)", "Communist Party of India (Marxist)"
  ),
  party_acronym = c(
    "CPI", "INC", "INC", "INC", "INC",
    "INC", "INC", "CPI(M)", "INC", "CPI(M)",
    "INC", "CPI(M)", "CPI(M)", "CPI(M)"
  )
)

# Data for Madhya Pradesh Legislative Assembly
state_party11 <- tibble(
  state = c(
    "Madhya Pradesh", "Madhya Pradesh", "Madhya Pradesh", "Madhya Pradesh", "Madhya Pradesh",
    "Madhya Pradesh", "Madhya Pradesh", "Madhya Pradesh", "Madhya Pradesh", "Madhya Pradesh",
    "Madhya Pradesh", "Madhya Pradesh", "Madhya Pradesh", "Madhya Pradesh"
  ),
  assembly_no = c(
    1, 2, 3, 4, 5,
    6, 7, 8, 9, 10,
    11, 12, 13, 14
  ),
  term_duration = c(
    "1952–1957", "1957–1962", "1962–1967", "1967–1972", "1972–1977",
    "1977–1980", "1980–1985", "1985–1990", "1990–1993", "1993–1998",
    "1998–2003", "2003–2008", "2008–2018", "2018–Present"
  ),
  ruling_party = c(
    "Indian National Congress", "Indian National Congress", "Indian National Congress",
    "Indian National Congress", "Indian National Congress", "Janata Party",
    "Indian National Congress", "Indian National Congress", "BJP", "Indian National Congress",
    "Indian National Congress", "BJP", "BJP", "Indian National Congress"
  ),
  party_acronym = c(
    "INC", "INC", "INC", "INC", "INC",
    "JP", "INC", "INC", "BJP", "INC",
    "INC", "BJP", "BJP", "INC"
  )
)


# Data for Maharashtra Legislative Assembly
state_party12 <- tibble(
  state = c(
    "Maharashtra", "Maharashtra", "Maharashtra", "Maharashtra", "Maharashtra",
    "Maharashtra", "Maharashtra", "Maharashtra", "Maharashtra", "Maharashtra",
    "Maharashtra", "Maharashtra", "Maharashtra"
  ),
  assembly_no = c(
    1, 2, 3, 4, 5,
    6, 7, 8, 9, 10,
    11, 12, 13
  ),
  term_duration = c(
    "1960–1962", "1962–1967", "1967–1972", "1972–1978", "1978–1980",
    "1980–1985", "1985–1990", "1990–1995", "1995–1999", "1999–2004",
    "2004–2009", "2009–2014", "2014–Present"
  ),
  ruling_party = c(
    "Indian National Congress", "Indian National Congress", "Indian National Congress",
    "Indian National Congress", "Indian National Congress", "Indian National Congress",
    "Indian National Congress", "Indian National Congress", "BJP-Shiv Sena",
    "Indian National Congress", "Indian National Congress", "BJP-Shiv Sena",
    "Shiv Sena-Congress-NCP Coalition"
  ),
  party_acronym = c(
    "INC", "INC", "INC", "INC", "INC",
    "INC", "INC", "INC", "BJP-SS", "INC",
    "INC", "BJP-SS", "SS-Cong-NCP"
  )
)


# Data for Punjab Legislative Assembly
# Data for Punjab Legislative Assembly
state_party13 <- tibble(
  state = c(
    "Punjab", "Punjab", "Punjab", "Punjab", "Punjab",
    "Punjab", "Punjab", "Punjab", "Punjab", "Punjab",
    "Punjab", "Punjab", "Punjab", "Punjab", "Punjab",
    "Punjab"
  ),
  assembly_no = c(
    1, 2, 3, 4, 5,
    6, 7, 8, 9, 10,
    11, 12, 13, 14, 15,
    16
  ),
  term_duration = c(
    "1952–1957", "1957–1962", "1962–1967", "1967–1969", "1969–1972",
    "1972–1977", "1977–1980", "1980–1985", "1985–1992", "1992–1997",
    "1997–2002", "2002–2007", "2007–2012", "2012–2017", "2017–2022",
    "2022–Present"
  ),
  ruling_party = c(
    "Indian National Congress", "Indian National Congress", "Indian National Congress",
    "Shiromani Akali Dal", "President’s Rule", "Indian National Congress",
    "Shiromani Akali Dal", "Indian National Congress", "President’s Rule",
    "Indian National Congress", "Shiromani Akali Dal-BJP", "Indian National Congress",
    "Shiromani Akali Dal-BJP", "Indian National Congress", "Indian National Congress",
    "Aam Aadmi Party"
  ),
  party_acronym = c(
    "INC", "INC", "INC", "SAD", "PR",
    "INC", "SAD", "INC", "PR", "INC",
    "SAD-BJP", "INC", "SAD-BJP", "INC",
    "INC", "AAP"
  )
)


# Data for Rajasthan Legislative Assembly
state_party14 <- tibble(
  state = c(
    "Rajasthan", "Rajasthan", "Rajasthan", "Rajasthan", "Rajasthan",
    "Rajasthan", "Rajasthan", "Rajasthan", "Rajasthan", "Rajasthan",
    "Rajasthan", "Rajasthan", "Rajasthan", "Rajasthan"
  ),
  assembly_no = c(
    1, 2, 3, 4, 5,
    6, 7, 8, 9, 10,
    11, 12, 13, 14
  ),
  term_duration = c(
    "1952–1957", "1957–1962", "1962–1967", "1967–1972", "1972–1977",
    "1977–1980", "1980–1985", "1985–1990", "1990–1993", "1993–1998",
    "1998–2003", "2003–2008", "2008–2013", "2013–Present"
  ),
  ruling_party = c(
    "Indian National Congress", "Indian National Congress", "Indian National Congress",
    "Indian National Congress", "Indian National Congress", "Janata Party",
    "Indian National Congress", "Indian National Congress", "BJP",
    "Indian National Congress", "Indian National Congress", "BJP",
    "Indian National Congress", "Indian National Congress"
  ),
  party_acronym = c(
    "INC", "INC", "INC", "INC", "INC",
    "JP", "INC", "INC", "BJP", "INC",
    "INC", "BJP", "INC", "INC"
  )
)


# Data for Tamil Nadu Legislative Assembly
state_party15 <- tibble(
  state = c(
    "Tamil Nadu", "Tamil Nadu", "Tamil Nadu", "Tamil Nadu", "Tamil Nadu",
    "Tamil Nadu", "Tamil Nadu", "Tamil Nadu", "Tamil Nadu", "Tamil Nadu",
    "Tamil Nadu", "Tamil Nadu", "Tamil Nadu", "Tamil Nadu"
  ),
  assembly_no = c(
    1, 2, 3, 4, 5,
    6, 7, 8, 9, 10,
    11, 12, 13, 14
  ),
  term_duration = c(
    "1952–1957", "1957–1962", "1962–1967", "1967–1971", "1971–1977",
    "1977–1980", "1980–1984", "1984–1989", "1989–1991", "1991–1996",
    "1996–2001", "2001–2006", "2006–2011", "2011–Present"
  ),
  ruling_party = c(
    "Indian National Congress", "Indian National Congress", "Indian National Congress",
    "Dravida Munnetra Kazhagam", "Dravida Munnetra Kazhagam", "AIADMK",
    "AIADMK", "AIADMK", "DMK", "AIADMK",
    "DMK", "AIADMK", "AIADMK", "DMK"
  ),
  party_acronym = c(
    "INC", "INC", "INC", "DMK", "DMK",
    "AIADMK", "AIADMK", "AIADMK", "DMK", "AIADMK",
    "DMK", "AIADMK", "AIADMK", "DMK"
  )
)


# Data for Uttar Pradesh Legislative Assembly
state_party16 <- tibble(
  state = c(
    "Uttar Pradesh", "Uttar Pradesh", "Uttar Pradesh", "Uttar Pradesh", "Uttar Pradesh",
    "Uttar Pradesh", "Uttar Pradesh", "Uttar Pradesh", "Uttar Pradesh", "Uttar Pradesh",
    "Uttar Pradesh", "Uttar Pradesh", "Uttar Pradesh", "Uttar Pradesh", "Uttar Pradesh",
    "Uttar Pradesh", "Uttar Pradesh"
  ),
  assembly_no = c(
    1, 2, 3, 4, 5,
    6, 7, 8, 9, 10,
    11, 12, 13, 14, 15,
    16, 17
  ),
  term_duration = c(
    "1952–1957", "1957–1962", "1962–1967", "1967–1969", "1969–1974",
    "1974–1977", "1977–1980", "1980–1985", "1985–1989", "1989–1991",
    "1991–1993", "1993–1996", "1996–2002", "2002–2007", "2007–2012",
    "2012–2017", "2017–Present"
  ),
  ruling_party = c(
    "Indian National Congress", "Indian National Congress", "Indian National Congress",
    "Indian National Congress", "Indian National Congress", "Indian National Congress",
    "Janata Party", "Indian National Congress", "Indian National Congress",
    "Janata Dal", "Bharatiya Janata Party", "Samajwadi Party",
    "Bharatiya Janata Party", "Bahujan Samaj Party", "Bahujan Samaj Party",
    "Samajwadi Party", "Bharatiya Janata Party"
  ),
  party_acronym = c(
    "INC", "INC", "INC", "INC", "INC",
    "INC", "JP", "INC", "INC", "JD",
    "BJP", "SP", "BJP", "BSP", "BSP",
    "SP", "BJP"
  )
)


# Data for Chhattisgarh Legislative Assembly
state_party17 <- tibble(
  state = c(
    "Chhattisgarh", "Chhattisgarh", "Chhattisgarh", "Chhattisgarh", "Chhattisgarh"
  ),
  assembly_no = c(
    1, 2, 3, 4, 5
  ),
  term_duration = c(
    "2000–2003", "2003–2008", "2008–2013", "2013–2018", "2018–Present"
  ),
  ruling_party = c(
    "Indian National Congress", "Bharatiya Janata Party", "Bharatiya Janata Party",
    "Bharatiya Janata Party", "Indian National Congress"
  ),
  party_acronym = c(
    "INC", "BJP", "BJP", "BJP", "INC"
  )
)


# Data for Jharkhand Legislative Assembly
state_party18 <- tibble(
  state = c(
    "Jharkhand", "Jharkhand", "Jharkhand", "Jharkhand", "Jharkhand"
  ),
  assembly_no = c(
    1, 2, 3, 4, 5
  ),
  term_duration = c(
    "2000–2005", "2005–2009", "2009–2014", "2014–2019", "2019–Present"
  ),
  ruling_party = c(
    "Bharatiya Janata Party", "Jharkhand Mukti Morcha", "Bharatiya Janata Party",
    "Bharatiya Janata Party", "Jharkhand Mukti Morcha"
  ),
  party_acronym = c(
    "BJP", "JMM", "BJP", "BJP", "JMM"
  )
)

# Data for Uttarakhand Legislative Assembly
state_party19 <- tibble(
  state = c(
    "Uttarakhand", "Uttarakhand", "Uttarakhand", "Uttarakhand", "Uttarakhand"
  ),
  assembly_no = c(
    1, 2, 3, 4, 5
  ),
  term_duration = c(
    "2000–2002", "2002–2007", "2007–2012", "2012–2017", "2017–Present"
  ),
  ruling_party = c(
    "Indian National Congress", "Indian National Congress", "Bharatiya Janata Party",
    "Indian National Congress", "Bharatiya Janata Party"
  ),
  party_acronym = c(
    "INC", "INC", "BJP", "INC", "BJP"
  )
)

# Data for West Bengal Legislative Assembly
state_party20 <- tibble(
  state = c(
    "West Bengal", "West Bengal", "West Bengal", "West Bengal", "West Bengal",
    "West Bengal", "West Bengal", "West Bengal", "West Bengal", "West Bengal",
    "West Bengal", "West Bengal", "West Bengal", "West Bengal", "West Bengal"
  ),
  assembly_no = c(
    1, 2, 3, 4, 5,
    6, 7, 8, 9, 10,
    11, 12, 13, 14, 15
  ),
  term_duration = c(
    "1952–1957", "1957–1962", "1962–1967", "1967–1969", "1969–1971",
    "1971–1977", "1977–1982", "1982–1987", "1987–1991", "1991–1996",
    "1996–2001", "2001–2006", "2006–2011", "2011–2016", "2016–Present"
  ),
  ruling_party = c(
    "Indian National Congress", "Indian National Congress", "Indian National Congress",
    "United Front", "President’s Rule", "Indian National Congress",
    "Communist Party of India (Marxist)", "Communist Party of India (Marxist)",
    "Communist Party of India (Marxist)", "Communist Party of India (Marxist)",
    "Communist Party of India (Marxist)", "Communist Party of India (Marxist)",
    "Communist Party of India (Marxist)", "All India Trinamool Congress",
    "All India Trinamool Congress"
  ),
  party_acronym = c(
    "INC", "INC", "INC", "UF", "PR",
    "INC", "CPI(M)", "CPI(M)", "CPI(M)", "CPI(M)",
    "CPI(M)", "CPI(M)", "CPI(M)", "AITC", "AITC"
  )
)

# Data for Telangana Legislative Assembly
state_party21 <- tibble(
  state = c(
    "Telangana", "Telangana", "Telangana"
  ),
  assembly_no = c(
    1, 2, 3
  ),
  term_duration = c(
    "2014–2018", "2018–2023", "2023–Present"
  ),
  ruling_party = c(
    "Telangana Rashtra Samithi", "Telangana Rashtra Samithi", "Bharat Rashtra Samithi"
  ),
  party_acronym = c(
    "TRS", "TRS", "BRS"
  )
)

library(dplyr)

# Generate a sequence of numbers from 1 to 21, excluding 6
state_party_numbers <- c(1:21) #setdiff(1:21, 6)

# Dynamically generate a list of data frames using mget()
state_party_list <- mget(paste0("state_party", state_party_numbers))

# Bind all rows together in a single step
state_ruling_party <- bind_rows(state_party_list) %>% 
  clean_names()





# 
# 
# # Add data for Chhattisgarh, Jharkhand, Uttarakhand, and West Bengal to the data frame
# state_party6 <- tibble(
#   state = c(
#     # Chhattisgarh
#     "Chhattisgarh", "Chhattisgarh", "Chhattisgarh", "Chhattisgarh", "Chhattisgarh",
#     # Jharkhand
#     "Jharkhand", "Jharkhand", "Jharkhand", "Jharkhand", "Jharkhand",
#     # Uttarakhand
#     "Uttarakhand", "Uttarakhand", "Uttarakhand", "Uttarakhand", "Uttarakhand",
#     # West Bengal
#     "West Bengal", "West Bengal", "West Bengal", "West Bengal", "West Bengal",
#     "West Bengal", "West Bengal", "West Bengal", "West Bengal", "West Bengal"
#   ),
#   assembly_no = c(
#     # Chhattisgarh
#     1, 2, 3, 4, 5,
#     # Jharkhand
#     1, 2, 3, 4, 5,
#     # Uttarakhand
#     1, 2, 3, 4, 5,
#     # West Bengal
#     1, 2, 3, 4, 5, 6, 7, 8, 9, 10
#   ),
#   term_duration = c(
#     # Chhattisgarh
#     "2000–2003", "2003–2008", "2008–2013", "2013–2018", "2018–Present",
#     # Jharkhand
#     "2000–2005", "2005–2009", "2009–2014", "2014–2019", "2019–Present",
#     # Uttarakhand
#     "2000–2002", "2002–2007", "2007–2012", "2012–2017", "2017–2022",
#     # West Bengal
#     "1952–1957", "1957–1962", "1962–1967", "1967–1969", "1969–1971",
#     "1971–1977", "1977–1982", "1982–1987", "1987–1991", "1991–1996"
#   ),
#   ruling_party = c(
#     # Chhattisgarh
#     "Indian National Congress", "Bharatiya Janata Party", "Bharatiya Janata Party",
#     "Bharatiya Janata Party", "Indian National Congress",
#     # Jharkhand
#     "Bharatiya Janata Party", "Jharkhand Mukti Morcha", "Bharatiya Janata Party",
#     "Bharatiya Janata Party", "Jharkhand Mukti Morcha",
#     # Uttarakhand
#     "Indian National Congress", "Indian National Congress", "Bharatiya Janata Party",
#     "Indian National Congress", "Bharatiya Janata Party",
#     # West Bengal
#     "Indian National Congress", "Indian National Congress", "Indian National Congress",
#     "United Front", "President’s Rule", "Indian National Congress",
#     "Communist Party of India (Marxist)", "Communist Party of India (Marxist)",
#     "Communist Party of India (Marxist)", "Communist Party of India (Marxist)"
#   ),
#   party_acronym = c(
#     # Chhattisgarh
#     "INC", "BJP", "BJP", "BJP", "INC",
#     # Jharkhand
#     "BJP", "JMM", "BJP", "BJP", "JMM",
#     # Uttarakhand
#     "INC", "INC", "BJP", "INC", "BJP",
#     # West Bengal
#     "INC", "INC", "INC", "UF", "PR", "INC", "CPI(M)", "CPI(M)", "CPI(M)", "CPI(M)"
#   )
# )
# 
# # Add data for Odisha and Telangana to the data frame
# state_party7 <- tibble(
#   state = c(
#     # Odisha
#     "Odisha", "Odisha", "Odisha", "Odisha", "Odisha",
#     "Odisha", "Odisha", "Odisha", "Odisha", "Odisha",
#     # Telangana
#     "Telangana", "Telangana", "Telangana"
#   ),
#   assembly_no = c(
#     # Odisha
#     1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
#     # Telangana
#     1, 2, 3
#   ),
#   term_duration = c(
#     # Odisha
#     "1952–1957", "1957–1962", "1962–1967", "1967–1971", "1971–1974",
#     "1974–1977", "1977–1980", "1980–1985", "1985–1990", "1990–1995",
#     # Telangana
#     "2014–2018", "2018–2023", "2023–Present"
#   ),
#   ruling_party = c(
#     # Odisha
#     "Indian National Congress", "Indian National Congress", "Indian National Congress",
#     "Indian National Congress", "Indian National Congress", "Indian National Congress",
#     "Janata Party", "Indian National Congress", "Janata Dal", "Biju Janata Dal",
#     # Telangana
#     "Telangana Rashtra Samithi", "Telangana Rashtra Samithi", "Bharat Rashtra Samithi"
#   ),
#   party_acronym = c(
#     # Odisha
#     "INC", "INC", "INC", "INC", "INC", "INC", "JP", "INC", "JD", "BJD",
#     # Telangana
#     "TRS", "TRS", "BRS"
#   )
# )
# 
# 
# state_party8 <- tibble(
#   state = c(
#     "Odisha", "Odisha", "Odisha", "Odisha", "Odisha",
#     "Odisha", "Odisha", "Odisha", "Odisha", "Odisha",
#     "Odisha", "Odisha", "Odisha"
#   ),
#   assembly_no = c(
#     1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
#     11, 12, 13
#   ),
#   term_duration = c(
#     "1952–1957", "1957–1962", "1962–1967", "1967–1971", "1971–1974",
#     "1974–1977", "1977–1980", "1980–1985", "1985–1990", "1990–1995",
#     "1995–2000", "2000–2004", "2004–Present"
#   ),
#   ruling_party = c(
#     "Indian National Congress", "Indian National Congress", "Indian National Congress",
#     "Indian National Congress", "Indian National Congress", "Indian National Congress",
#     "Janata Party", "Indian National Congress", "Janata Dal", "Janata Dal",
#     "Indian National Congress", "Biju Janata Dal", "Biju Janata Dal"
#   ),
#   party_acronym = c(
#     "INC", "INC", "INC", "INC", "INC", "INC", "JP", "INC", "JD", "JD",
#     "INC", "BJD", "BJD"
#   )
# )
# 
# 
# sp=state_party1 %>% bind_rows(state_party2) %>% 
#   bind_rows(state_party3) %>% 
#   bind_rows(state_party4) %>%
#   bind_rows(state_party5) %>%
#   bind_rows(state_party6) %>%
#   bind_rows(state_party8) %>%
#   clean_names()
# 
