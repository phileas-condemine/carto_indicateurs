SP=readLines("synonymes/SP500.txt")
# View(SP)

pivot=setdiff(1:length(SP),stringr::str_which(SP,"^ "))
synonymes=stringr::str_which(SP,"^ (EP|EM)")

to_keep=c(pivot,synonymes)
to_keep=sort(to_keep)

SP=SP[to_keep]
SP=data.frame(txt=SP)
SP$statut="pivot"
SP[stringr::str_which(SP$txt,"^ (EP|EM)"),]$statut <- "synonyme"

SP$statut_top=lead(SP$statut)

SP=SP[!SP$statut==SP$statut_top|SP$statut=="synonyme",]
SP$txt=gsub("^ (EP|EM)","",SP$txt)
SP$txt=gsub("^ ","",SP$txt)
SP$txt=tolower(SP$txt)
SP$is_pivot=(SP$statut=="pivot")*1
SP$id=cumsum(SP$is_pivot)
load("data/init_data2.RData")
indicateurs=index$Indicateur
indicateurs=tolower(indicateurs)

pivots=SP[SP$statut=="pivot",c("txt","id")]
synonymes=SP[SP$statut=="synonyme",c("txt","id")]

rel=merge(pivots,synonymes,by="id")
names(rel) <- c("id","pivot","synonyme")

mapping=pbapply::pblapply(paste0("(^| )(",SP$txt,")( |$)"),function(term){
  stringr::str_which(indicateurs,term)
})
names(mapping) <- SP$txt

mapping_sub=mapping[lapply(mapping,length)>0]
save(mapping_sub,file = "synonymes/bdsp500_utilité_indicateurs_fullword.RData")

useful_sp=stack(lapply(mapping_sub,length))
useful_sp=unique(useful_sp)
useful_sp$ind=as.character(useful_sp$ind)
names(useful_sp) <- c("nb_pivot","term")
rel=merge(rel,useful_sp,by.x="pivot",by.y="term",all.x=T)
names(useful_sp) <- c("nb_syn","term")
rel=merge(rel,useful_sp,by.x="synonyme",by.y="term",all.x=T)

really_useful=na.omit(rel)

really_useful=really_useful[!str_detect(really_useful$synonyme,really_useful$pivot),]
really_useful=really_useful[!str_detect(really_useful$pivot,really_useful$synonyme),]

really_useful <- really_useful %>% 
  rowwise() %>%
  mutate(word1=sort(c(synonyme,pivot))[1],
         word2=sort(c(synonyme,pivot))[2],
         freq1=pmax(nb_syn,nb_pivot),
         freq2=pmin(nb_syn,nb_pivot))%>%
  select(word1,word2,freq1,freq2)%>%
  distinct()

fwrite(really_useful,file="synonymes/synonymes_bdsp_utiles.csv")

# On se rend compte que les acronymes peuvent 
# correspondre à des morceaux de mots et donc 
# amu => samu, joint => conjoint, faux mapping !