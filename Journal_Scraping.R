# This extra credit project is used to find the detalis of several journals over the years. 
# The jornal that we have chosen to scrape is https://www.g3journal.org/ 
# This collaborative project was done by 
# 
# Ashwin Alex -  aa2734
# Rahul Basu  -  rb622
# Seyedeh Mirrahimi - sm2754
# Shawn Rahul D Souza - srd59


library(stringr)
library(rvest)
library(dplyr)
#library(xlsx)

#Read the year 
readinteger <- function()
{ 
  n <- readline(prompt="Enter a year: ")
  if(!grepl("^[0-9]+$",n))
  {
    return(readinteger())
  }
  
  return(as.integer(n))
}



months_this_year <- function(site)   # Extract the months of this journal year 
{
  site_year = read_html(site)
  site_year
  years_node = html_nodes(site_year, ".highwire-cite-metadata")
  years_site = html_text(years_node) %>% length() 
  years_site=years_site/4
  return(years_site)
  
}

monthly_page_list <- vector() #The total list of journals

j <- readinteger()
j <- j%%10 #input variable for the first year
for(i in j:9)
{   
  month <- months_this_year( str_c("https://www.g3journal.org/content/",i))
  str_c("https://www.g3journal.org/content/",i)
  list_of_pages <- str_c("https://www.g3journal.org/content/",i,"/",1:month,"/") #extracting all journals year i
  monthly_page_list <- c(monthly_page_list,list_of_pages)
}



extract_months <- function(monthly_page_list)
{
  url_vec = vector()
  for (monthly_page in monthly_page_list)
  {
    site_month = xml2::read_html(monthly_page)
    sitesno = html_nodes(site_month, ".issue-toc-section-investigations .toc-section , .last.even .highwire-cite-metadata , .issue-toc-section-mutant-screen-reports , .issue-toc-section-genome-reports")
    sites = str_trim(html_text(sitesno))
    sites_val = str_extract_all(sites, 'https://doi.org/\\S+')
    sites_val
    class(sites)
    
    for(i in sites_val)
    {   str_trim(i)
      i
      url_vec <- c(url_vec,i)
    }
    url_vec
    
  }
  
  return(url_vec)
}

journals_for_a_month <- extract_months(monthly_page_list)

number_of_journals_in_month <- length(journals_for_a_month)

doi_text <- c()
title_text <- c()
authors_text <- c()
publication_date_text <- c()
abstract_text<- c()
keywords_text<- c()
full_text_text<- c()
affiliation_text<- c()
corresponding_text<- c()
email_text<- c()

failed_attempts <- c("https://doi.org/10.1534/g3.118.201003", "https://doi.org/10.1534/g3.111.000265", "https://doi.org/10.1534/g3.112.004440","https://doi.org/10.1534/g3.112.004630", "https://doi.org/10.1534/g3.118.200697","https://doi.org/10.1534/g3.118.200858","https://doi.org/10.1534/g3.118.200075","https://www.g3journal.org/content/8/5/1659","https://doi.org/10.1534/g3.118.200253")

for(journal in journals_for_a_month)
{  if (journal %in% failed_attempts == FALSE)
{
  site = read_html(journal)
  print(site)
  no = html_nodes(site, ".highwire-citation-highwire-article-top-a")
  print(no)
  doi <-  html_nodes(site, ".highwire-cite-metadata")%>%  html_text(no)
  
  doi_text <- c(doi_text,doi[7])
  
  title <-  html_nodes(site, "#page-title")%>%  html_text(no)
  title_text <- c(title_text,title[1])
  
  authors <-  html_nodes(site, ".highwire-cite-authors")%>%  html_text(no)
  authors_text <- c(authors_text,authors[1])
  
  url <- journal 
  all_links <- paste(readLines(url), collapse="\n") # extract all the links present in the page source
  matched_links <- str_match(all_links, "https://www.g3journal.org/content/\\S+") # match the link we want using the regex
  
  new_url = matched_links[[1]][1] #the links are in a list, the first element always has the actual url we want
  new_url = str_replace(new_url, '\"', '.article-info') #add the postfix
  #extract affiliation using this code
  site_data = read_html(new_url)
  
  affiliation= html_nodes(site_data, ".affiliation-list") %>%  html_text()
  affiliation_text <- c(affiliation_text,affiliation)
  
  #extract the paragraph which has corresp authot and email
  corresponding = html_nodes(site_data, "#corresp-1") %>%  html_text()
  corresponding_text <- c(corresponding_text,corresponding)
  
  #extract email using the regex
  email = str_extract_all(corresponding, "E-mail: \\S+", simplify = TRUE) %>% substring(8)
  email_text <- c(email_text,email)
  
  
  publication_date <-  html_nodes(site, ".highwire-cite-metadata")%>%  html_text(no)
  publication_date_text <- c(publication_date_text,publication_date[3])
  
  abstract <-  html_nodes(site, "#abstract-1")%>%  html_text(no)
  abstract_text <- c(abstract_text,abstract)
  
  keywords <-  html_nodes(site, ".kwd")%>%  html_text()
  keyword <- paste(keywords,sep='  ',collapse = ', ')
  keywords_text <- c(keywords_text,keyword)
  
  full_text <-  html_nodes(site, ".fulltext-view , .highwire-citation-highwire-article-top-a")%>%  html_text(no)
  texts <- paste(full_text,sep='  ',collapse = ' ')
  texts <- str_replace_all(texts, "[\r\n\t]" , " ")
  full_text_text <- c(full_text_text,texts)
}    
}


write.table(doi_text,file="doi.txt",append = TRUE)
write.table(title_text,file="title.txt",append = TRUE)
write.table(authors_text,file="authors.txt",append = TRUE)
write.table(publication_date_text,file="publication_date.txt",append = TRUE)
write.table(abstract_text,file="abstract.txt",append = TRUE)
write.table(keywords_text,file="keywords.txt",append = TRUE)
write.table(full_text_text,file="all.txt",append = TRUE)

write.table(affiliation_text,file="affiliation.txt",append = TRUE)
write.table(corresponding_text,file="correspondence.txt",append = TRUE)
write.table(email_text,file="correspondece_email.txt",append = TRUE)

n= max(length(doi_text),length(title_text),length(affiliation_text),length(authors_text),length(email_text),length(publication_date_text),length(abstract_text),length(keywords_text),length(full_text_text))
length(doi_text) <- n
length(title_text)<- n
length(authors_text)<- n
length(affiliation_text)<- n
length(authors_text)<- n
length(email_text)<- n
length(publication_date_text)<- n
length(abstract_text)<- n
length(keywords_text)<- n
length(full_text_text)<- n

journal_fields <- data.frame(doi_text,title_text,authors_text, affiliation_text,authors_text,email_text, publication_date_text,abstract_text,keywords_text,full_text_text)
names(journal_fields) <- c("DOI","AUTHORS","TITLE","AUTHOR_AFFILIATIONS","CORRESPONDENCE_AUTHORS","CORRESPONDENCE_AUTHORS_EMAILS","PUBLICATION DATE","ABSTRACT","KEYWORDS","FULL TEXT")
write.table(journal_fields, file = "Project_Info_1.txt",row.names=FALSE,col.names=TRUE,na="NA",append = TRUE)
write.xlsx(journal_fields, file = "Project_Info_11.xlsx",row.names=FALSE,col.names=TRUE,append = TRUE)
