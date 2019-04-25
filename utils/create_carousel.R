# logos_dispos=list.files("www/logo_producteurs/")
# logos_dispos=data.table(logos=logos_dispos)
# producteurs_bases=fread("data/producteurs.csv")
logos_dispos=fread("data/producteurs.csv")
setnames(logos_dispos,"img","logos")
logos_dispos=logos_dispos[!logos==""]
# logos_dispos=merge(logos_dispos,producteurs_bases,by.x="logos",by.y="img",all.x=F,all.y=F)
# logos_dispos[,order_random:=sample(.N)]
# setorder(logos_dispos,order_random)

logos_dispos=logos_dispos[!href==""]

logos_dispos[,img_gdrive:=gsub(pattern = "open",replacement = "uc",x = img_gdrive)]
local_to_gdrive=logos_dispos$img_gdrive
names(local_to_gdrive)=paste0("logo_producteurs/",logos_dispos$logos)

HTML2CHAR=function(file){
  res=paste(readLines(paste0("www/text_producteurs/",file),encoding = "UTF-8"),collapse=" ")
  res=paste("<html><body>",res,"</body></html>")
  # res=gsub("logo_producteurs","www/logo_producteurs",res)
  res=stringr::str_replace_all(res,local_to_gdrive)
  res=gsub("<img ","<img style='display: inline;max-width:30%;max-height:100px;width: auto;height: auto;' ",res)
}
html_producteurs=unique(logos_dispos$html)
html_producteurs=html_producteurs[!html_producteurs==""]
contents2=unlist(lapply(html_producteurs,HTML2CHAR))
contents2=data.table(html=html_producteurs,content=contents2)
setkey(contents2,html)
setkey(logos_dispos,html)
# img_corres=logos_dispos[html_producteurs]$logos
img_corres=logos_dispos[html_producteurs]$img_gdrive

all_contents=logos_dispos[html_producteurs]$html
all_contents=contents2[all_contents]$content

rand=sample(length(all_contents))
all_contents=all_contents[rand]
img_corres=img_corres[rand]

cP=readLines("www/style_dot_imgs.js")

cP=htmlwidgets::JS(cP)


# print(all_contents[1:2])

s1 <- slickR(
  obj = all_contents,
  dotObj = img_corres,slideType = 'iframe',
  slickOpts = list(
    initialSlide = 0,
    slidesToShow = 1,
    slidesToScroll = 1,
    focusOnSelect = T,
    dots = T,
    customPaging = cP
  ),height = 500,width='100%'
)

#Putting it all together in one slickR call
s2 <- htmltools::tags$script(
  sprintf("var dotObj = %s", jsonlite::toJSON(img_corres))
)

carousel <- htmltools::tagList(s2, s1)
# htmltools::browsable(carousel)




# slickR(obj = all_contents,dotObj = img_corres,slideType = 'iframe')
# https://drive.google.com/uc?authuser=0&id=1L0V0PKwmvfm6YHIeJZIYuqDpBbnxo3Og&export=download
# https://drive.google.com/file/d/1L0V0PKwmvfm6YHIeJZIYuqDpBbnxo3Og/view
