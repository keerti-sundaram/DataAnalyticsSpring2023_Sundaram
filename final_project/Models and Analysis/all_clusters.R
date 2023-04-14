gdp_1 <- c('Albania', 'Antigua and Barbuda', 'Armenia', 'Azerbaijan', 'Bahamas', 'Bahrain', 'Brunei Darussalam', 'China, Hong Kong SAR', 'Cyprus', 'Dominican Republic', 'Egypt', 'Fiji', 'Georgia', 'Grenada', 'Guatemala', 'Guyana', 'Iran (Islamic Republic of)', 'Iraq', 'Kazakhstan', 'Kuwait', 'Kyrgyzstan', 'Mauritius', 'Mexico', 'Mongolia', 'Paraguay', 'Peru', 'Philippines', 'Saint Kitts and Nevis', 'Saint Lucia', 'Saint Vincent and the Grenadines', 'Singapore', 'Sri Lanka', 'Suriname', 'Syrian Arab Republic', 'Tajikistan', 'Thailand', 'Trinidad and Tobago', 'Turkmenistan', 'Uzbekistan')
gdp_2 <- c('Barbados', 'Belarus', 'Belize', 'Brazil', 'Bulgaria', 'Cabo Verde', 'Chile', 'Dominica', 'Ecuador', 'El Salvador', 'Estonia', 'Israel', 'Jamaica', 'Jordan', 'Latvia', 'Lebanon', 'Lithuania', 'Maldives', 'Nicaragua', 'Panama', 'Poland', 'Republic of Korea', 'Republic of Moldova', 'Romania', 'Russian Federation', 'Seychelles', 'South Africa', 'Switzerland', 'Turkey', 'Ukraine')
gdp_3 <- c('Austria', 'Belgium', 'Canada', 'Cuba', 'Denmark', 'Finland', 'France', 'Germany', 'Iceland', 'Japan', 'New Zealand', 'Norway', 'Sweden', 'United States of America')
gdp_4 <- c('Argentina', 'Australia', 'Bosnia and Herzegovina', 'Colombia', 'Costa Rica', 'Croatia', 'Czechia', 'Greece', 'Hungary', 'Ireland', 'Italy', 'Luxembourg', 'Malta', 'Montenegro', 'Netherlands', 'Portugal', 'Serbia', 'Slovakia', 'Slovenia', 'Spain', 'Uruguay')

copd_1 <- c('Canada', 'Colombia', 'France', 'Italy', 'Japan', 'Kazakhstan', 'Mexico', 'Philippines', 'South Africa', 'Spain', 'Turkey', 'Ukraine')
copd_2 <- c('United States of America')
copd_3 <- c('Albania', 'Antigua and Barbuda', 'Argentina', 'Armenia', 'Australia', 'Austria', 'Azerbaijan', 'Bahamas', 'Bahrain', 'Barbados', 'Belarus', 'Belgium', 'Belize', 'Bosnia and Herzegovina', 'Brunei Darussalam', 'Bulgaria', 'Cabo Verde', 'Chile', 'China, Hong Kong SAR', 'Costa Rica', 'Croatia', 'Cuba', 'Cyprus', 'Czechia', 'Denmark', 'Dominica', 'Dominican Republic', 'Ecuador', 'Egypt', 'El Salvador', 'Estonia', 'Fiji', 'Finland', 'Georgia', 'Greece', 'Grenada', 'Guatemala', 'Guyana', 'Hungary', 'Iceland', 'Iran (Islamic Republic of)', 'Iraq', 'Ireland', 'Israel', 'Jamaica', 'Jordan', 'Kuwait', 'Kyrgyzstan', 'Latvia', 'Lebanon', 'Lithuania', 'Luxembourg', 'Maldives', 'Malta', 'Mauritius', 'Mongolia', 'Montenegro', 'Netherlands', 'New Zealand', 'Nicaragua', 'Norway', 'Panama', 'Paraguay', 'Peru', 'Poland', 'Portugal', 'Republic of Korea', 'Republic of Moldova', 'Romania', 'Saint Kitts and Nevis', 'Saint Lucia', 'Saint Vincent and the Grenadines', 'Serbia', 'Seychelles', 'Singapore', 'Slovakia', 'Slovenia', 'Sri Lanka', 'Suriname', 'Sweden', 'Switzerland', 'Syrian Arab Republic', 'Tajikistan', 'Thailand', 'Trinidad and Tobago', 'Turkmenistan', 'Uruguay', 'Uzbekistan')
copd_4 <- c('Brazil', 'Germany', 'Russian Federation')

#"Kazakhstan" (copd_1),"Mexico" (copd_1),"Philippines" (copd_1) -> in gdp
cat((setdiff(gdp_1, copd_3)), sep="\n")
#"Brazil" (copd_4),"Russian Federation" (copd_4),"South Africa" (copd_1),"Turkey" (copd_1),"Ukraine" (copd_1) 
setdiff(gdp_2, copd_3)
#"Canada","France","Germany" (copd_4),Japan","United States of America" (copd_2)
setdiff(gdp_3, copd_3)
#"Colombia" (copd_1),"Italy" (copd_1),"Spain" (copd_1)
setdiff(gdp_4, copd_3)

intersect(gdp_1, copd_1)
intersect(gdp_4, copd_1)

intersect(gdp_2, copd_1)
intersect(gdp_2, copd_4)
intersect(gdp_3, copd_1)
intersect(gdp_3, copd_4)
intersect(gdp_3, copd_2)
