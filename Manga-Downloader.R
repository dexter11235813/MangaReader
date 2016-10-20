library(rvest)
library(magrittr)
library(stringr)
library(Rcompression)
library(gtools)


main = function()
{
  name = readline("Manga Name \n")
  chapter_start = as.integer(readline("From Chapter number \n"))
  chapter_end = as.integer(readline("To Chapter number \n"))
  if(chapter_end == "") 
    {
    chapter_end = chapter_start
  }
  for(i in chapter_start:chapter_end)
  {
    manga.download(name,i)
  }
}

# Directory name - to name the directory
manga.name = function(name)
{
  name = unlist(strsplit(name," "))
  dirname = paste(sapply(name,function(x) return(paste(c(toupper(substring(x,1,1)),substring(x,2,)),collapse =""))),collapse = " ")
  return(dirname)
}



# Adjust page numbers
adjusted.page.no = function(i)
{
  if(i %in% 1:9)
  {
    return( paste(c("00",as.character(i)),collapse = ""))
  }else 
  {
    if(i %in% 10:99)
    {
      return( paste(c("0",as.character(i)),collapse = ""))
    }
  else
  {
    return(as.character(i))
  }
  
}
}
manga.download = function(name,chapter)
{
  temp = name
  name = paste(unlist(strsplit(tolower(name)," ")),collapse = "-")
  chapter_name = as.character(chapter)
  url_root = "http://www.mangapanda.com/"
  download_stem = "/home/abhijit331/Dropbox/Manga/"
  download_loc = paste(c(download_stem,manga.name(temp)),collapse ="")
  #setwd(download_stem)
  if(file.exists(download_loc)) {
    setwd(download_loc)
  } else
  {
    dir.create(paste(c(download_loc,"/"),collapse = ""))
    setwd(paste(c(download_loc,"/"),collapse = ""))
  }
  download_loc = paste(c(download_loc,"/",paste(c("Chapter-",chapter_name),collapse ="")),collapse="")
  print(download_loc)
  if(file.exists(download_loc)) 
    {
    stop('The chapter already exists')
  }else
  {
    dir.create(paste(c("Chapter-",chapter_name),collapse = ""))
    setwd(paste(c("Chapter-",chapter_name),collapse = ""))
  
  manga_url = paste(c(url_root,name,"/",chapter_name),collapse = "")
  manga_url.html = read_html(manga_url)
  pages = manga_url.html %>% html_node("#selectpage") %>% html_text()
  tot.pages = as.integer(unlist(strsplit(pages,split = " "))[3])
  for(i in 1:tot.pages)
  {
    current_page = paste(c(manga_url,"/",as.character(i)),collapse = "")
    current_page.html = read_html(current_page)
    img = current_page.html %>% html_node("#img") %>% html_attr("src")
    pageno = paste0("page",adjusted.page.no(i))
    
    download.file(img,paste0(pageno,".jpg"))
  }
  wordict = getwd()
  prefix = substring(wordict,17,)
  list.of.files = list.files()
  path.for.compression = unlist(lapply(list.of.files,function(x) return(paste0("~/",prefix,"/",x))))
  Rcompression::zip(paste(c(manga.name(temp),paste(c("Chapter-",chapter_name),collapse =""),".cbz"),collapse = ""),path.for.compression)
  }

}


main()
